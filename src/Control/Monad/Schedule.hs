{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

{-| Run scheduled computations in any (stateful) monad, using an adapter.

This module mostly contains utilities for dealing with clock inputs. To get or
set the existing timeouts, use your 'RunSched' adapter on one of the functions
from "Data.Schedule", which this module also re-exports.
-}
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
import           Control.Monad.Extra    (whenMaybe)
import           Data.Either            (either)
import           Data.Functor.Identity  (Identity (..))
import           Data.Maybe             (fromMaybe)

-- internal
import           Data.Schedule
import           Data.Schedule.Internal


-- | Something that can run 'Schedule' state transition functions.
--
-- This could be pure (e.g. 'Control.Monad.Trans.State.Strict.StateT') or
-- impure (e.g. reference to a 'Control.Monad.Primitive.Extra.PrimST').
--
-- Examples:
--
-- @
--    primState :: PrimMonad m => RunSched t (ReaderT (PrimST m (Schedule t)) m)
--    primState sched = asks statePrimST >>= \run -> lift (run sched)
--
--    state :: Monad m => RunSched t (StateT (Schedule t) m)
--    zoom _lens . state :: Monad m => RunSched t (StateT s m)
-- @
--
-- See the unit tests for more examples.
type RunSched t m = forall a . (Schedule t -> (a, Schedule t)) -> m a

runTick :: (Monad m, Monoid a) => RunSched t m -> (t -> m a) -> m a
runTick runS runTickTask = whileJustM $ do
  runS popOrTick >>= \case
    Nothing     -> pure Nothing
    Just (t, p) -> do
      runS $ modST $ acquireTask (t, p)
      r <- runTickTask p -- TODO: catch Haskell exceptions here
      runS $ modST $ releaseTask t
      pure (Just r)

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

-- | A more general version of 'mkOutput' that uses a
-- 'Control.Lens.Prism.Prism'-like optic.
--
-- Given an inner computation @it -> m a@ where one branch of the @it@ type has
-- a @('Tick', t)@ tuple representing individual input tasks, return an outer
-- computation of type @i -> m a@ where the @i@ type only has a 'Tick'. When
-- the outer computation receives these 'Tick' inputs, it automatically
-- resolves the relevant tasks of type @t@ that are active for that 'Tick', and
-- passes each tuple in sequence to the wrapped inner computation.
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
