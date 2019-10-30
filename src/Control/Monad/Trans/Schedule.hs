{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

module Control.Monad.Trans.Schedule
  ( ScheduleT(..)
  , runScheduleT
  , tickNow
  , tickPrev
  , ticksToIdle
  , schedule
  , schedule'
  , doWhileAccum
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

-- | Get the current tick, whose tasks have not all run yet.
tickNow :: Monad m => ScheduleT t m Tick
tickNow = ScheduleT (now <$> get)

-- | Get the previous tick, whose tasks have all already run.
tickPrev :: Monad m => ScheduleT t m Tick
tickPrev = pred <$> tickNow

ticksToIdle :: Monad m => ScheduleT t m (Maybe TickDelta)
ticksToIdle = ScheduleT (ticksUntilNextTask <$> get)

-- | Run a schedule action like 'after', 'cancel', or 'renew'.
schedule :: Monad m => (Schedule t -> (a, Schedule t)) -> ScheduleT t m a
schedule = ScheduleT . state

-- | Run a schedule modification like 'acquireLiveTask' or 'releaseLiveTask'.
schedule' :: Monad m => (Schedule t -> Schedule t) -> ScheduleT t m ()
schedule' = ScheduleT . modify

doWhileAccum :: (Monad m, Monoid a) => m (Maybe a) -> m a
doWhileAccum act = go mempty
 where
  go accum = act >>= \case
    Just r  -> go (accum <> r)
    Nothing -> pure accum

runTick :: (Monad m, Monoid a) => (t -> ScheduleT t m a) -> ScheduleT t m a
runTick runTask = doWhileAccum $ do
  schedule popOrTick >>= maybe (pure Nothing) \(c, t) -> do
    schedule' $ acquireLiveTask c
    r <- runTask t -- TODO: catch Haskell exceptions here
    schedule' $ releaseLiveTask c
    pure $ Just r

runTicksTo
  :: (Monad m, Monoid a) => (t -> ScheduleT t m a) -> Tick -> ScheduleT t m a
runTicksTo runTask tick = doWhileAccum $ do
  tick' <- tickNow
  whenMaybe (tick' < tick) $ runTick runTask

getInput
  :: (Monad m)
  => (TickDelta -> m (Either Tick i))
  -> ScheduleT t m (Either Tick i)
getInput getTimedInput = do
  d <- ticksToIdle
  lift $ getTimedInput (fromMaybe maxBound d)

mkOutput
  :: (Monad m, Monoid a)
  => (t -> ScheduleT t m a)
  -> (i -> ScheduleT t m a)
  -> (Either Tick i -> ScheduleT t m a)
mkOutput runTask runInput = runTicksTo runTask `either` runInput
