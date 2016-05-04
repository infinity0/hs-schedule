{-# LANGUAGE RankNTypes #-}

{-| Run a pure scheduled computation impurely via 'MonadBase' -}

module Control.Monad.Trans.Schedule.Base (
    baseLiftClock
  , getClockNow
  , runTasks
  , runScheduleT
  ) where

-- external
import Control.Monad.Base (MonadBase(liftBase))

-- ours
import Control.Monad.Base.Compose ()
import Control.Monad.Trans.Schedule.Internal


-- | Lift a clock computation at the base of the monad transformer stack.
baseLiftClock :: MonadBase c m => LiftClock c m
baseLiftClock = liftBase

-- | As 'getClockNow'' but without the need for an explicit 'LiftClock'.
getClockNow :: MonadBase c m => ScheduleT c m Tick
getClockNow = getClockNow' baseLiftClock

-- | As 'runTasks'' but without the need for an explicit 'LiftClock'.
runTasks :: MonadBase c m => ScheduleT c m ()
runTasks = runTasks' baseLiftClock

-- | As 'runScheduleT'' but without the need for an explicit 'LiftClock'.
runScheduleT :: MonadBase c m => ScheduleT c m a -> Clock c -> m (a, TaskState c m)
runScheduleT = runScheduleT' baseLiftClock
