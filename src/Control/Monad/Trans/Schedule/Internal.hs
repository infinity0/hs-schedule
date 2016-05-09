{-# LANGUAGE
    RankNTypes
  , MultiParamTypeClasses
  #-}

{-| Pure scheduled computations, as a monad transformer. -}

module Control.Monad.Trans.Schedule.Internal (
    Tick
  , Clock(..)
  -- * Pure scheduled computation
  , ScheduleT
  , Task
  , TaskCancel
  , TaskState
  , getClock
  , tickNow
  , tickPrev
  , runTasksTo
  , runScheduleT_
  , after
  , renew
  -- * Generic impure execution
  , LiftClock
  , MonadClock(..)
  , defaultLiftClock
  , LiftRT
  , MonadRT(..)
  , defaultLiftRT
  , getClockNow'
  , runTasks'
  , runScheduleT'
  ) where

-- external
import Control.Exception (assert)
import Control.Monad (unless)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Compose (ComposeT(..))
import Control.Monad.Trans.Reader (ReaderT(..), ask)
import Control.Monad.Trans.State.Strict (StateT(..), get)

-- ours
import Control.Clock (Tick, Clock(..), checkPos)
import Data.Rsv.RRelMMap (RRelMMap)
import qualified Data.Rsv.RRelMMap as RelM


{-| A computation that can schedule sub-computations for later.

    We use 'ComposeT' so we need only one 'lift' from the inner monad @m@.

    TODO: There is an 'MFunctor' instance for the underlying type 'ComposeT',
    but that's not what we want here - a hoist must also morph the 'TaskState'.
    So we should probably wrap this in a newtype and define a proper 'MFunctor'
    instance that isn't just 'hoist' from 'ComposeT'. However, this is probably
    impossible with how 'ScheduleT' is currently defined; see
    <src/Control-Monad-Trans-Schedule-ExampleMFunctor.html Control.Monad.Trans.Schedule.ExampleMFunctor>
    for details.

    The solution would likely involve adding a 's' type parameter for the state
    that is independent of 'm'. This would increase complexity; however a
    'MFunctor' instance is quite important for this monad to be composeable
    with other monads.
-}
type ScheduleT c m = ComposeT
    (ReaderT (Clock c))        -- maybe-impure clock
    (StateT (TaskState c m))   -- pure representation of pending tasks
    m                          -- underlying computation

{-| A task to run, in response to a tick event. This can be any computation.

    In other programming contexts, this would be analogous to a callback,
    subscriber, observer, timeout, etc etc.
-}
type Task c m = ScheduleT c m ()

{-| Cancel a task.

    /Which/ task to cancel, is implicitly bound to each instance of this type.
    See 'after' for more details.
-}
type TaskCancel c m = ScheduleT c m (Maybe (Task c m))

-- | The state of all scheduled pending tasks.
newtype TaskState c m = TS { relM :: RRelMMap Tick (Task c m) }
-- We had to make this a newtype to break the cycle of type synonyms.
-- Fortunately, liftTS' helps us keep the boilerplate down.

-- TODO: there's probably a cleaner way to express this, maybe with Lens
toTS :: Functor m => StateT (RRelMMap Tick (Task c m)) m a -> StateT (TaskState c m) m a
toTS rms = StateT $ \ts -> fmap TS <$> runStateT rms (relM ts)

liftTS :: Monad m => StateT (TaskState c m) m a -> ScheduleT c m a
liftTS = ComposeT . lift

liftTS' :: Monad m => StateT (RRelMMap Tick (Task c m)) m a -> ScheduleT c m a
liftTS' = liftTS . toTS

getClock :: Monad m => ScheduleT c m (Clock c)
getClock = ComposeT ask

-- | Get the current tick, whose tasks have not yet run.
tickNow :: Monad m => ScheduleT c m Tick
tickNow = RelM.current <$> liftTS' get

-- | Get the previous tick, whose tasks have all already run.
tickPrev :: Monad m => ScheduleT c m Tick
tickPrev = pred <$> tickNow

runTasksAt :: Monad m => Tick -> ScheduleT c m ()
runTasksAt tick = do
    oldTS <- liftTS' get
    let tick' = assert (RelM.current oldTS == tick) tick
    -- TODO: could support dynamic update of the "tasks for this tick" to allow
    -- 'after 0' to work. but that's quite complicated so leave it out for now
    sequence_ $ oldTS RelM.! tick'
    liftTS' $ do
        RelM.sDeleteKey tick
        RelM.sSetCurrent (tick + 1)

runTasksTo :: Monad m => Tick -> ScheduleT c m ()
runTasksTo tick = do
    -- TODO: emit a warning if the real clock is not monotonic.
    -- note that TaskState is already monotonic, so we are "safe" in the sense
    -- that we'll never execute past tasks, but the behaviour would better if
    -- the real clock had a monotonic wrapper that smooths out time jumps.
    curTS <- liftTS' get
    unless (RelM.isEmpty curTS) $ -- short-cut calculation, for runScheduleT'
      sequence_ $ runTasksAt <$> [RelM.current curTS .. tick - 1]

-- | Run a scheduled computation starting from tick 0.
runScheduleT_ :: Monad m => ScheduleT c m a -> Clock c -> m (a, TaskState c m)
runScheduleT_ schedule clock =
    getComposeT schedule `runReaderT` clock `runStateT` TS RelM.empty

-- | Schedule a task to run after a given number of ticks.
after :: Monad m => Tick -> Task c m -> ScheduleT c m (TaskCancel c m)
after t a = liftTS' <$> liftTS' (RelM.sInsertAfter (checkPos t) a)

-- | Re-schedule a task to instead run after a given number of ticks.
-- If the task was already cancelled, do nothing.
renew :: Monad m => Tick -> TaskCancel c m -> ScheduleT c m (Maybe (TaskCancel c m))
renew t cancel = cancel >>= \prev -> case prev of
    Nothing -> return Nothing
    Just task -> Just <$> after t task

{-| Lift a clock computation into the scheduled computation's context.

    We use a type synonym instead of a typeclass, so that we can avoid
    overlapping instances such as these:

    > instance MonadBase c m => MonadClock c m (ScheduleT c m)
    > instance MonadIO m => MonadClock IO m (ScheduleT IO m)

    but still write generic code like 'runScheduleT'' to be useful externally.
-}
type LiftClock c m = forall a. c a -> m a

-- | A monad that can lift clock operations.
class MonadClock c m where
    liftClock :: LiftClock c m

-- | Helps to derive new instances of 'MonadClock' from base instances.
defaultLiftClock :: (MonadClock c m, Monad m, MonadTrans t) => LiftClock c (t m)
defaultLiftClock = lift . liftClock

-- | Lift some deeply-inner computation @n@ into a scheduled computation,
-- running tasks in parallel while the computation is still pending.
type LiftRT c n m = forall a. n a -> ScheduleT c m a

-- | A monad that can lift inner computations to run tasks in parallel.
class MonadRT c b m where
    liftRT :: LiftRT c b m

hoist' :: Monad m => (forall a. m a -> n a) -> ScheduleT c m b -> ScheduleT c n b
hoist' morph m = undefined -- probably impossible, see doc for ScheduleT
-- if we ever define this, we'd wrap ScheduleT in a newtype and define the following:
-- instance MFunctor (ScheduleT c) where
--     hoist = hoist'

-- | Helps to derive new instances of 'MonadRT' from base instances.
-- Don't use this yet, it's undefined right now.
defaultLiftRT :: (MonadRT c b m, Monad m, MonadTrans t) => LiftRT c b (t m)
defaultLiftRT = hoist' lift . liftRT

-- | Get the time from the clock.
getClockNow' :: Monad m => LiftClock c m -> ScheduleT c m Tick
getClockNow' liftClock' = getClock >>= lift . liftClock' . clockNow

-- | Run tasks up to but not including the current clock tick.
runTasks' :: Monad m => LiftClock c m -> ScheduleT c m ()
runTasks' liftClock' = getClockNow' liftClock' >>= runTasksTo

-- | Run a scheduled computation, starting from the current clock time.
runScheduleT' :: Monad m => LiftClock c m -> ScheduleT c m a -> Clock c -> m (a, TaskState c m)
runScheduleT' liftClock' schedule = runScheduleT_ $ runTasks' liftClock' >> schedule
-- TODO: maybe we need to keep running this until TaskState is empty; test this
