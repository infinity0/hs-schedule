{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Data.Schedule.Internal where

-- external
import           Data.Bifunctor  (first)
import           Data.Text       (Text, pack)
import           Data.Word       (Word64)
import           GHC.Generics    (Generic)
import           GHC.Stack       (HasCallStack)

-- internal
import qualified Data.Map.Strict as M
import qualified Data.Rsv.RMMap  as RM
import qualified Data.Set        as S

import           Data.Rsv.RMMap  (RMMap)


type Tick = Integer
type TickDelta = Word64

-- | A task that is currently or was part of a schedule.
--
-- @t@ is the type of input parameter for each task, i.e. the task contents.
newtype Task t = Task (RM.Delete Tick t)
  deriving (Show, Read, Generic, Eq, Ord)

-- | The current status of a task as returned by 'taskStatus'.
data TaskStatus t =
    TaskNotPending
  -- ^ The task is not pending - either it was already run, or cancelled.
  | TaskPending !Tick !t
  -- ^ The task is due to run at some future tick.
  | TaskRunning !t
  -- ^ The task is running right now.
  deriving (Show, Read, Generic, Eq, Ord)

-- | The state of all scheduled pending tasks.
--
-- @t@ is the type of task-params.
data Schedule t = Schedule {
    now     :: !Tick
  , tasks   :: !(RMMap Tick t)
  , pending :: !(S.Set (Task t))
  , running :: !(Maybe (Task t, t))
} deriving (Show, Read, Generic, Eq)

newSchedule :: Schedule t
newSchedule =
  Schedule { now = 0, tasks = RM.empty, pending = mempty, running = Nothing }

-- | Check the schedule that its internal invariants all hold.
--
-- You must run this after deserialising one from untrusted input, e.g. via the
-- 'Read' or 'Generic' instance. A result of "Nothing" means the check passed,
-- otherwise 'Just errmsg' is given back.
checkValidity :: Schedule t -> Maybe Text
checkValidity Schedule {..} =
  let tasksValid = RM.checkValidity tasks
      tasks'     = RM.content tasks
      nowMatch   = case M.lookupMin tasks' of
        Nothing                -> True
        Just (nextTaskTick, _) -> now <= nextTaskTick
      pending' = S.fromList $ Task <$> RM.toList tasks
  in  case tasksValid of
        Just e -> Just e
        Nothing
          | not nowMatch        -> Just $ pack "has tasks for before now"
          | pending /= pending' -> Just $ pack "inconsistent pending tasks"
          | otherwise           -> Nothing

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
ticksToIdle :: Schedule t -> Maybe TickDelta
ticksToIdle Schedule {..} = do
  (m, _) <- M.lookupMin (RM.content tasks)
  pure (fromIntegral (m - now))

taskStatus :: HasCallStack => Task t -> Schedule t -> TaskStatus t
taskStatus t@(Task d) Schedule {..} = if S.member t pending
  then case RM.unqueue d tasks of -- ofc this doesn't actually unqueue
    (Nothing             , _) -> error "inconsistent pending tasks"
    (Just (tick, tParams), _) -> TaskPending tick tParams
  else case running of
    Just (t', tParams) | t == t' -> TaskRunning tParams
    _                            -> TaskNotPending

-- | Schedule a task to run after a given number of ticks.
--
-- This is relative to 'tickNow'; a @0@ delta schedules the task to be run at
-- the end of the current tick, i.e. as soon as possible but not immediately.
after :: TickDelta -> t -> Schedule t -> (Task t, Schedule t)
after tDelta tParams s0@(Schedule now tasks0 pending0 _) =
  let tick        = now + toInteger tDelta
      (d, tasks1) = RM.enqueue (tick, tParams) tasks0
      pending1    = S.insert (Task d) pending0
  in  (Task d, s0 { tasks = tasks1, pending = pending1 })

-- | Cancel a task. Result is Nothing if task was not already pending.
cancel :: Task t -> Schedule t -> (Maybe t, Schedule t)
cancel (Task d) s0@(Schedule _ tasks0 pending0 _) = case RM.unqueue d tasks0 of
  (Nothing, _) -> (Nothing, s0)
  (Just (_, tParams), tasks1) ->
    let pending1 = S.delete (Task d) pending0
    in  (Just tParams, s0 { tasks = tasks1, pending = pending1 })

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
  (Just (_, tParams), tasks1) ->
    first Just $ after tDelta tParams (s0 { tasks = tasks1 })

-- | Pop the next task to be run in this tick.
-- If there are no more tasks remaining, then advance to the next tick.
popOrTick :: HasCallStack => Schedule t -> (Maybe (Task t, t), Schedule t)
popOrTick s0@(Schedule now0 tasks0 pending0 running) = case running of
  Just _  -> error "tried to pop tick while task was running"
  Nothing -> case RM.dequeue now0 tasks0 of
    (Nothing, _) -> (Nothing, s0 { now = succ now0 })
    (Just (d, tParams), tasks1) ->
      let pending1 = S.delete (Task d) pending0
      in  (Just (Task d, tParams), s0 { tasks = tasks1, pending = pending1 })

-- | Lock the schedule before running a particular task.
--
-- This prevents popOrTick from being called, or other tasks from running.
-- It is not re-entrant; only one task is supposed to run at once.
acquireTask :: HasCallStack => (Task t, t) -> Schedule t -> Schedule t
acquireTask k s = case running s of
  Just _ -> error "tried to acquire on unreleased task"
  _      -> s { running = Just k }

-- | Unlock the schedule after running a particular task.
--
-- This allows popOrTick to be called again and other tasks to run.
-- It is not re-entrant; only one task is supposed to run at once.
releaseTask :: HasCallStack => Task t -> Schedule t -> Schedule t
releaseTask t s = case running s of
  Just (t', _) | t' == t -> s { running = Nothing }
  _                      -> error "tried to release on unacquired task"
