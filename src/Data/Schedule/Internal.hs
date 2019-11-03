{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

module Data.Schedule.Internal where

-- external
import           Data.Bifunctor                 ( first )
import           Data.Word                      ( Word64 )
import           GHC.Generics                   ( Generic )

-- internal
import qualified Data.Map.Strict               as Map
import           Data.Rsv.RMMap                 ( RMMap )
import qualified Data.Rsv.RMMap                as M


-- TODO: export to upstream extra
whileJustM :: (Monad m, Monoid a) => m (Maybe a) -> m a
whileJustM act = go mempty
 where
  go accum = act >>= \case
    Just r  -> go (accum <> r)
    Nothing -> pure accum

type Tick = Integer
type TickDelta = Word64

-- | A task currently part of a schedule.
--
-- 't' is the type of task-params, i.e. the input data for the task.
newtype LiveTask t = LiveTask (M.Delete Tick t)
  deriving (Eq, Show, Generic)

-- | The state of all scheduled pending tasks.
--
-- 't' is the type of task-params.
data Schedule t = Schedule {
    now        :: !Tick
  -- ^ The current tick, whose tasks have not all run yet.
  , tasks      :: !(RMMap Tick t)
  , nowRunning :: !(Maybe (LiveTask t))
  -- TODO: map of all LiveTasks, so we can check the status of a LiveTask
} deriving (Eq, Show, Generic)

newSchedule :: Schedule t
newSchedule = Schedule { now = 0, tasks = M.empty, nowRunning = Nothing }

-- | Get the current tick.
tickNow :: Schedule t -> Tick
tickNow = now

-- | Get the previous tick.
tickPrev :: Schedule t -> Tick
tickPrev = pred . now

-- | Get the number of ticks until the next scheduled task.
--
-- This may be used by an impure computation to set an actual timeout. Note
-- that the timeout should be interruptible e.g. in case some other input
-- arrives in the meantime to set an earlier timeout.
ticksToIdle :: Schedule t -> Maybe TickDelta
ticksToIdle (Schedule now' tasks0 _) = do
  m <- fst <$> Map.lookupMin (M.content tasks0)
  let d = (m - now')
  if d < 0 then error "minimum key is in the past???" else pure (fromIntegral d)

-- | Schedule a task to run after a given number of ticks.
after :: TickDelta -> t -> Schedule t -> (LiveTask t, Schedule t)
after tDelta task s@(Schedule now' tasks0 _) =
  let tick = now' + toInteger tDelta
  in  let (d, tasks1) = M.enqueue (tick, task) tasks0
      in  (LiveTask d, s { tasks = tasks1 })

-- | Cancel a task.
cancel :: LiveTask t -> Schedule t -> (Maybe t, Schedule t)
cancel (LiveTask c) sched0 = case M.unqueue c (tasks sched0) of
  (Nothing, _     ) -> (Nothing, sched0)
  (Just k , tasks1) -> (Just k, sched0 { tasks = tasks1 })

-- | Re-schedule a task to instead run after a given number of ticks.
-- If the task was already cancelled, do nothing.
renew
  :: TickDelta -> LiveTask t -> Schedule t -> (Maybe (LiveTask t), Schedule t)
renew tDelta (LiveTask c) sched0 = case M.unqueue c (tasks sched0) of
  (Nothing, _     ) -> (Nothing, sched0)
  (Just k , tasks1) -> first Just $ after tDelta k (sched0 { tasks = tasks1 })

-- | Pop the next task to be run in this tick.
-- If there are no more tasks remaining, then advance to the next tick.
popOrTick :: Schedule t -> (Maybe (LiveTask t, t), Schedule t)
popOrTick s@(Schedule now' tasks0 _) = case M.dequeue now' tasks0 of
  (Just (c, t), tasks1) -> (Just (LiveTask c, t), s { tasks = tasks1 })
  (Nothing    , tasks1) -> (Nothing, s { now = succ now', tasks = tasks1 })

-- | Lock the schedule before running a particular live task.
acquireLiveTask :: LiveTask t -> Schedule t -> Schedule t
acquireLiveTask c s = case nowRunning s of
  Just c' | c' /= c -> error "tried to acquire inconsistent live task"
  _                 -> s { nowRunning = Just c }

-- | Unlock the schedule after running a particular live task.
releaseLiveTask :: LiveTask t -> Schedule t -> Schedule t
releaseLiveTask c s = case nowRunning s of
  Just c' | c' == c -> s { nowRunning = Nothing }
  _                 -> error "tried to release inconsistent live task"
