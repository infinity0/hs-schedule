{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Data.Schedule.Internal where

-- external
import           Data.Bifunctor  (first)
import           Data.Word       (Word64)
import           GHC.Generics    (Generic)
import           GHC.Stack       (HasCallStack)

-- internal
import qualified Data.Map.Strict as M
import qualified Data.Rsv.RMMap  as RM

import           Data.Rsv.RMMap  (RMMap)


type Tick = Integer
type TickDelta = Word64

-- | A task that is currently or was part of a schedule.
--
-- @t@ is the type of input parameter for each task, i.e. the task contents.
newtype Task t = Task (RM.Delete Tick t)
  deriving (Show, Read, Generic, Eq, Ord)

-- | The state of all scheduled pending tasks.
--
-- @t@ is the type of task-params.
data Schedule t = Schedule {
    now     :: !Tick
  , tasks   :: !(RMMap Tick t)
  , running :: !(Maybe (Task t))
  -- TODO: map of all tasks, so we can check the status of a task
} deriving (Show, Read, Generic, Eq)

newSchedule :: Schedule t
newSchedule = Schedule { now = 0, tasks = RM.empty, running = Nothing }

-- | Check the schedule that its internal invariants all hold.
--
-- You must run this after deserialising one from untrusted input, e.g. via the
-- 'Read' or 'Generic' instance. A result of "Nothing" means the check passed,
-- otherwise 'Just errmsg' is given back.
checkValidity :: Schedule t -> Maybe String
checkValidity Schedule {..} =
  let nowMatch = case M.lookupMin (RM.content tasks) of
        Nothing                -> True
        Just (nextTaskTick, _) -> now <= nextTaskTick
  in  if nowMatch then Nothing else Just "has tasks for before now"

-- | Get the current tick, whose tasks have not all run yet.
--
-- From the perspective of the pure computation that is running this schedule,
-- you should treat this as the current "logical time", even if an impure clock
-- is telling you that the "environment time" is in the future.
tickNow :: Schedule t -> Tick
tickNow = now

-- | Get the previous tick, whose tasks have all already run.
tickPrev :: Schedule t -> Tick
tickPrev = pred . now

-- | Get the number of ticks until the next scheduled task.
--
-- This may be used by an impure runtime environment to set an actual timeout;
-- see 'Control.Clock' for details.
ticksToIdle :: HasCallStack => Schedule t -> Maybe TickDelta
ticksToIdle Schedule {..} = do
  m <- fst <$> M.lookupMin (RM.content tasks)
  let d = m - now
  if d < 0 then error "minimum key is in the past???" else pure (fromIntegral d)

-- | Schedule a task to run after a given number of ticks.
--
-- This is relative to 'tickNow'; a @0@ delta schedules the task to be run at
-- the end of the current tick, i.e. as soon as possible but not immediately.
after :: TickDelta -> t -> Schedule t -> (Task t, Schedule t)
after tDelta tParams s0@(Schedule now tasks0 _) =
  let tick        = now + toInteger tDelta
      (d, tasks1) = RM.enqueue (tick, tParams) tasks0
  in  (Task d, s0 { tasks = tasks1 })

-- | Cancel a task. Result is Nothing if task was not already pending.
cancel :: Task t -> Schedule t -> (Maybe t, Schedule t)
cancel (Task d) s0 = case RM.unqueue d (tasks s0) of
  (Nothing     , _     ) -> (Nothing, s0)
  (Just tParams, tasks1) -> (Just tParams, s0 { tasks = tasks1 })

-- | Cancel a task, discarding the result.
cancel_ :: Task t -> Schedule t -> ((), Schedule t)
cancel_ t s = ((), snd $ cancel t s)

-- | Reschedule a pending task to instead run after a given number of ticks.
--
-- If the task was not already pending, do nothing. If you need to reschedule
-- a task unconditionally even if it was already cancelled or run, use both
-- 'cancel_' and 'after' in combination.
renew :: TickDelta -> Task t -> Schedule t -> (Maybe (Task t), Schedule t)
renew tDelta (Task d) s0 = case RM.unqueue d (tasks s0) of
  (Nothing, _) -> (Nothing, s0)
  (Just tParams, tasks1) ->
    first Just $ after tDelta tParams (s0 { tasks = tasks1 })

-- | Pop the next task to be run in this tick.
-- If there are no more tasks remaining, then advance to the next tick.
popOrTick :: HasCallStack => Schedule t -> (Maybe (Task t, t), Schedule t)
popOrTick s0@(Schedule now0 tasks0 running) = case running of
  Just _  -> error "tried to pop tick while task was running"
  Nothing -> case RM.dequeue now0 tasks0 of
    (Nothing, _) -> (Nothing, s0 { now = succ now0 })
    (Just (d, tParams), tasks1) ->
      (Just (Task d, tParams), s0 { tasks = tasks1 })

-- | Lock the schedule before running a particular task.
--
-- This prevents popOrTick from being called, or other tasks from running.
-- It is not re-entrant; only one task is supposed to run at once.
acquireTask :: HasCallStack => Task t -> Schedule t -> Schedule t
acquireTask t s = case running s of
  Just _ -> error "tried to acquire on unreleased task"
  _      -> s { running = Just t }

-- | Unlock the schedule after running a particular task.
--
-- This allows popOrTick to be called again and other tasks to run.
-- It is not re-entrant; only one task is supposed to run at once.
releaseTask :: HasCallStack => Task t -> Schedule t -> Schedule t
releaseTask t s = case running s of
  Just t' | t' == t -> s { running = Nothing }
  _                 -> error "tried to release on unacquired task"
