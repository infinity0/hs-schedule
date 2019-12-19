{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes     #-}

{-| Run scheduled computations in any (stateful) monad, using an adapter. -}
module Control.Monad.Schedule
  ( RunSched
  , runTick
  , runTicksTo
  , getInput
  , mkOutput
  , tickTask
  , module Data.Schedule
  )
where

-- external
import           Control.Monad.Extra       (whenMaybe)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import           Data.Either               (either)
import           Data.Functor.Identity     (Identity (..))
import           Data.Maybe                (fromMaybe)

-- internal
import           Data.Schedule
import           Data.Schedule.Internal


-- | Something that can run @Schedule@ state transitions.
--
-- This could be pure (e.g. @StateT@) or impure (e.g. reference to a @PrimST@).
--
-- Examples:
--
-- @
--    runSchedMV :: PrimMonad m => RunSched t (ReaderT (PrimST m (Schedule t)) m)
--    runSchedMV sched = asks statePrimST >>= \run -> lift (run sched)
--
--    state :: Monad m => RunSched t (StateT (Schedule t) m)
--    zoom _lens . state :: Monad m => RunSched t (StateT s m)
-- @
--
-- See the unit tests for more examples.
type RunSched t m = forall a . (Schedule t -> (a, Schedule t)) -> m a

runTick :: (Monad m, Monoid a) => RunSched t m -> (t -> m a) -> m a
runTick runS runTickTask = whileJustM $ runMaybeT $ do
  MaybeT (runS popOrTick) >>= \(t, p) -> lift $ do
    runS $ modST $ acquireTask (t, p)
    r <- runTickTask p -- TODO: catch Haskell exceptions here
    runS $ modST $ releaseTask t
    pure r

runTicksTo
  :: (Monad m, Monoid a) => RunSched t m -> (Tick -> t -> m a) -> Tick -> m a
runTicksTo runS runTask tick = whileJustM $ do
  tick' <- runS $ getST tickNow
  whenMaybe (tick' < tick) $ runTick runS $ runTask tick

getInput
  :: (Monad m)
  => RunSched t m
  -> (TickDelta -> m (Either Tick i))
  -> m (Either Tick i)
getInput runS getTimedInput = do
  d <- runS $ getST ticksToIdle
  getTimedInput (fromMaybe maxBound d)

mkOutput
  :: (Monad m, Monoid a)
  => RunSched t m
  -> (Tick -> t -> m a)
  -> (i -> m a)
  -> (Either Tick i -> m a)
mkOutput runS runTask runInput = runTicksTo runS runTask `either` runInput

-- | A more general version of mkOutput that uses a prism-like optic.
--
-- Given an input executor @it -> m a@ where one branch of the @it@ type has
-- a @(Tick, t)@ tuple that represents individual input tasks, return a
-- convenience wrapper executor of type @i -> m a@ where the @i@ type only
-- has a @Tick@. When the wrapper executor receives these @Tick@ inputs, it
-- automatically resolves the relevant tasks of type @t@ that are active for
-- that @Tick@, and passes each tuple in sequence to the wrapped executor.
tickTask
  :: (Monad m, Monoid a)
  => RunSched t m
  -> (forall f . Applicative f => (Tick -> f (Tick, t)) -> i -> f it)
  -> (it -> m a)
  -> (i -> m a)
tickTask runS prism runTaskOr input = case prism Left input of
  Right it   -> runTaskOr it
  Left  tick -> runTicksTo runS (fmap runTaskOr . inputWithTask) tick
  where inputWithTask t k = runIdentity (prism (const (pure (t, k))) input)
