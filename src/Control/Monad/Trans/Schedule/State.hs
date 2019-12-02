{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes     #-}

{-| Like 'Control.Monad.Trans.Schedule' but with a plain 'StateT'.

Meant to be used with 'Control.Lens.zoom'
-}
module Control.Monad.Trans.Schedule.State
  ( schedule
  , schedule'
  , getSched
  , runTick
  , runTicksTo
  , getInput
  , mkOutput
  , tickTask
  , module Control.Monad.Trans.Class
  , module Data.Schedule
  )
where

-- external
import           Control.Monad.Extra              (whenMaybe)
import           Control.Monad.Trans.Class        (MonadTrans (lift))
import           Control.Monad.Trans.Maybe        (MaybeT (MaybeT, runMaybeT))
import           Control.Monad.Trans.State.Strict (StateT, get, modify, state)
import           Data.Either                      (either)
import           Data.Functor.Identity            (Identity (..))
import           Data.Maybe                       (fromMaybe)

-- internal
import           Data.Schedule
import           Data.Schedule.Internal


-- | Run a schedule action like 'after', 'cancel', or 'renew'.
schedule
  :: Monad m => (Schedule t -> (a, Schedule t)) -> StateT (Schedule t) m a
schedule = state

-- | Run a schedule modification like 'acquireLiveTask' or 'releaseLiveTask'.
schedule' :: Monad m => (Schedule t -> Schedule t) -> StateT (Schedule t) m ()
schedule' = modify

-- | Get a schedule property like 'tickNow', 'tickPrev', or 'ticksToIdle'.
getSched :: Monad m => (Schedule t -> a) -> StateT (Schedule t) m a
getSched f = f <$> get

runTick
  :: (Monad m, Monoid a, Monad n)
  => (forall x . StateT (Schedule t) m x -> n x)
  -> (t -> n a)
  -> n a
runTick liftS runTickTask = whileJustM $ runMaybeT $ do
  MaybeT (liftS $ schedule popOrTick) >>= \(c, t) -> lift $ do
    liftS $ schedule' $ acquireLiveTask c
    r <- runTickTask t -- TODO: catch Haskell exceptions here
    liftS $ schedule' $ releaseLiveTask c
    pure r

runTicksTo
  :: (Monad m, Monoid a, Monad n)
  => (forall x . StateT (Schedule t) m x -> n x)
  -> (Tick -> t -> n a)
  -> Tick
  -> n a
runTicksTo liftS runTask tick = whileJustM $ do
  tick' <- liftS $ getSched tickNow
  whenMaybe (tick' < tick) $ runTick liftS $ runTask tick

getInput
  :: (Monad m)
  => (TickDelta -> m (Either Tick i))
  -> StateT (Schedule t) m (Either Tick i)
getInput getTimedInput = do
  d <- getSched ticksToIdle
  lift $ getTimedInput (fromMaybe maxBound d)

mkOutput
  :: (Monad m, Monoid a, Monad n)
  => (forall x . StateT (Schedule t) m x -> n x)
  -> (Tick -> t -> n a)
  -> (i -> n a)
  -> (Either Tick i -> n a)
mkOutput liftS runTask runInput = runTicksTo liftS runTask `either` runInput

-- | A more general version of mkOutput that uses a prism-like optic.
--
-- Given an input executor 'it -> n a' where one branch of the 'it' type has
-- a '(Tick, t)' tuple that represents individual input tasks, return a
-- convenience wrapper executor of type 'i -> n a' where the 'i' type only
-- has a 'Tick'. When the wrapper executor receives these 'Tick' inputs, it
-- automatically resolves the relevant tasks of type 't' that are active for
-- that 'Tick', and passes each tuple in sequence to the wrapped executor.
tickTask
  :: (Monad m, Monoid a, Monad n)
  => (forall x . StateT (Schedule t) m x -> n x)
  -> (forall f . Applicative f => (Tick -> f (Tick, t)) -> i -> f it)
  -> (it -> n a)
  -> (i -> n a)
tickTask liftS prism runTaskOr input = case prism Left input of
  Right it   -> runTaskOr it
  Left  tick -> runTicksTo liftS (fmap runTaskOr . inputWithTask) tick
  where inputWithTask t k = runIdentity (prism (const (pure (t, k))) input)
