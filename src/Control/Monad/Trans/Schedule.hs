{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Trans.Schedule
  ( ScheduleT(..)
  , runScheduleT
  , schedule
  , schedule'
  , getSched
  , runTick
  , runTicksTo
  , getInput
  , mkOutput
  , module Control.Monad.Trans.Class
  , module Data.Schedule
  )
where

-- external
import           Control.Applicative            ( Alternative )
import           Control.Monad                  ( MonadPlus )
import           Control.Monad.Extra            ( whenMaybe )
import           Control.Monad.Trans.Class      ( MonadTrans(lift) )
import           Control.Monad.Trans.Maybe      ( MaybeT(MaybeT, runMaybeT) )
import           Control.Monad.Trans.State.Strict
                                                ( StateT(runStateT)
                                                , get
                                                , modify
                                                , state
                                                )
import           Data.Either                    ( either )
import           Data.Maybe                     ( fromMaybe )

-- internal
import           Data.Schedule
import           Data.Schedule.Internal


{-| A computation that can schedule sub-computations for later. -}
newtype ScheduleT t m a = ScheduleT { unScheduleT :: StateT (Schedule t) m a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadTrans)

runScheduleT :: ScheduleT t m a -> Schedule t -> m (a, Schedule t)
runScheduleT = runStateT . unScheduleT

-- | Run a schedule action like 'after', 'cancel', or 'renew'.
schedule :: Monad m => (Schedule t -> (a, Schedule t)) -> ScheduleT t m a
schedule = ScheduleT . state

-- | Run a schedule modification like 'acquireLiveTask' or 'releaseLiveTask'.
schedule' :: Monad m => (Schedule t -> Schedule t) -> ScheduleT t m ()
schedule' = ScheduleT . modify

-- | Get a schedule property like 'tickNow', 'tickPrev', or 'ticksToIdle'.
getSched :: Monad m => (Schedule t -> a) -> ScheduleT t m a
getSched f = ScheduleT (f <$> get)

runTick :: (Monad m, Monoid a) => (t -> ScheduleT t m a) -> ScheduleT t m a
runTick runTickTask = whileJustM $ runMaybeT $ do
  MaybeT (schedule popOrTick) >>= \(c, t) -> lift $ do
    schedule' $ acquireLiveTask c
    r <- runTickTask t -- TODO: catch Haskell exceptions here
    schedule' $ releaseLiveTask c
    pure r

runTicksTo
  :: (Monad m, Monoid a)
  => (Tick -> t -> ScheduleT t m a)
  -> Tick
  -> ScheduleT t m a
runTicksTo runTask tick = whileJustM $ do
  tick' <- getSched tickNow
  whenMaybe (tick' < tick) $ runTick $ runTask tick

getInput
  :: (Monad m)
  => (TickDelta -> m (Either Tick i))
  -> ScheduleT t m (Either Tick i)
getInput getTimedInput = do
  d <- getSched ticksToIdle
  lift $ getTimedInput (fromMaybe maxBound d)

mkOutput
  :: (Monad m, Monoid a)
  => (Tick -> t -> ScheduleT t m a)
  -> (i -> ScheduleT t m a)
  -> (Either Tick i -> ScheduleT t m a)
mkOutput runTask runInput = runTicksTo runTask `either` runInput
