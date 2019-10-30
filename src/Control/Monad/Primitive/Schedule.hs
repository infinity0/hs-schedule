{-# LANGUAGE BlockArguments             #-}

module Control.Monad.Primitive.Schedule
  ( tickNow
  , tickPrev
  , ticksToIdle
  , schedule
  , schedule'
  , runTick
  , runTicksTo
  , getInput
  , mkOutput
  , module Control.Monad.Primitive.Extra
  , module Data.Schedule
  )
where

-- external
import           Control.Monad.Trans.Class      ( MonadTrans(lift) )
import           Control.Monad.Trans.Maybe      ( MaybeT(MaybeT, runMaybeT) )
import           Control.Monad.Primitive.Extra  ( PrimMonad
                                                , PrimST(statePrimST)
                                                , readPrimST
                                                , modifyPrimST
                                                )
import           Control.Monad.Extra            ( whenMaybe )
import           Data.Maybe                     ( fromMaybe )

-- internal
import           Data.Schedule
import           Data.Schedule.Internal


-- | Get the current tick, whose tasks have not all run yet.
tickNow :: PrimMonad m => PrimST m (Schedule t) -> m Tick
tickNow sched = now <$> readPrimST sched

-- | Get the previous tick, whose tasks have all already run.
tickPrev :: PrimMonad m => PrimST m (Schedule t) -> m Tick
tickPrev sched = pred <$> tickNow sched

ticksToIdle :: PrimMonad m => PrimST m (Schedule t) -> m (Maybe TickDelta)
ticksToIdle sched = ticksUntilNextTask <$> readPrimST sched

-- | Run a schedule action like 'after', 'cancel', or 'renew'.
schedule
  :: PrimMonad m
  => PrimST m (Schedule t)
  -> (Schedule t -> (a, Schedule t))
  -> m a
schedule sched = statePrimST sched

-- | Run a schedule modification like 'acquireLiveTask' or 'releaseLiveTask'.
schedule'
  :: PrimMonad m => PrimST m (Schedule t) -> (Schedule t -> Schedule t) -> m ()
schedule' sched = modifyPrimST sched

runTick :: (PrimMonad m, Monoid a) => PrimST m (Schedule t) -> (t -> m a) -> m a
runTick sched runTask = whileJustM $ runMaybeT $ do
  MaybeT (schedule sched popOrTick) >>= \(c, t) -> lift $ do
    schedule' sched $ acquireLiveTask c
    r <- runTask t -- TODO: catch Haskell exceptions here
    schedule' sched $ releaseLiveTask c
    pure r

runTicksTo
  :: (PrimMonad m, Monoid a)
  => PrimST m (Schedule t)
  -> (t -> m a)
  -> Tick
  -> m a
runTicksTo sched runTask tick = whileJustM $ do
  tick' <- tickNow sched
  whenMaybe (tick' < tick) $ runTick sched runTask

getInput
  :: (PrimMonad m)
  => PrimST m (Schedule t)
  -> (TickDelta -> m (Either Tick i))
  -> m (Either Tick i)
getInput sched getTimedInput = do
  d <- ticksToIdle sched
  getTimedInput (fromMaybe maxBound d)

mkOutput
  :: (PrimMonad m, Monoid a)
  => PrimST m (Schedule t)
  -> (t -> m a)
  -> (i -> m a)
  -> (Either Tick i -> m a)
mkOutput sched runTask runInput = runTicksTo sched runTask `either` runInput
