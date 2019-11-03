{-# LANGUAGE BlockArguments             #-}

module Control.Monad.Primitive.Schedule
  ( schedule
  , schedule'
  , getSched
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


-- | Run a schedule action like 'after', 'cancel', or 'renew'.
schedule :: PrimST m (Schedule t) -> (Schedule t -> (a, Schedule t)) -> m a
schedule sched = statePrimST sched

-- | Run a schedule modification like 'acquireLiveTask' or 'releaseLiveTask'.
schedule' :: PrimST m (Schedule t) -> (Schedule t -> Schedule t) -> m ()
schedule' sched = modifyPrimST sched

-- | Get a schedule property like 'tickNow', 'tickPrev', or 'ticksToIdle'.
getSched :: Functor m => PrimST m (Schedule t) -> (Schedule t -> a) -> m a
getSched sched f = f <$> readPrimST sched

runTick :: (PrimMonad m, Monoid a) => PrimST m (Schedule t) -> (t -> m a) -> m a
runTick sched runTickTask = whileJustM $ runMaybeT $ do
  MaybeT (schedule sched popOrTick) >>= \(c, t) -> lift $ do
    schedule' sched $ acquireLiveTask c
    r <- runTickTask t -- TODO: catch Haskell exceptions here
    schedule' sched $ releaseLiveTask c
    pure r

runTicksTo
  :: (PrimMonad m, Monoid a)
  => PrimST m (Schedule t)
  -> (Tick -> t -> m a)
  -> Tick
  -> m a
runTicksTo sched runTask tick = whileJustM $ do
  tick' <- getSched sched tickNow
  whenMaybe (tick' < tick) $ runTick sched $ runTask tick

getInput
  :: (PrimMonad m)
  => PrimST m (Schedule t)
  -> (TickDelta -> m (Either Tick i))
  -> m (Either Tick i)
getInput sched getTimedInput = do
  d <- getSched sched ticksToIdle
  getTimedInput (fromMaybe maxBound d)

mkOutput
  :: (PrimMonad m, Monoid a)
  => PrimST m (Schedule t)
  -> (Tick -> t -> m a)
  -> (i -> m a)
  -> (Either Tick i -> m a)
mkOutput sched runTask runInput = runTicksTo sched runTask `either` runInput
