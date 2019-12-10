{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TupleSections              #-}

{-| Run scheduled computations in a (stateful) arrow.

TODO: generalise this in the style of Control.Monad.Schedule and move this
simply to Control.Arrow.Schedule.
-}
module Control.Arrow.Transformer.Schedule
  ( ScheduleArr(..)
  , runScheduleArr
  , schedule
  , schedule'
  , getSched
  , runTick
  , runTicksTo
  , getInput
  , mkOutput
  , module Control.Arrow.Transformer
  , module Data.Schedule
  )
where

-- external
import           Control.Arrow                   (Arrow (..), ArrowApply (app),
                                                  ArrowChoice ((|||)),
                                                  ArrowLoop, ArrowPlus,
                                                  ArrowZero, returnA)
import           Control.Arrow.Operations        (ArrowState (fetch))
import           Control.Arrow.Transformer       (ArrowTransformer (lift))
import           Control.Arrow.Transformer.State (StateArrow (StateArrow),
                                                  runState)
import           Control.Category                (Category, (<<<), (>>>))
import           Data.Maybe                      (fromMaybe)

-- internal
import           Data.Schedule
import           Data.Schedule.Internal


{-| A computation that can schedule sub-computations for later. -}
newtype ScheduleArr t a i o = ScheduleArr { unScheduleArr :: StateArrow (Schedule t) a i o }
  deriving (Category, Arrow, ArrowZero, ArrowPlus, ArrowChoice, ArrowLoop)

instance ArrowApply a => ArrowApply (ScheduleArr t a) where
  app = ScheduleArr
    (StateArrow
      (arr (\((ScheduleArr (StateArrow f), x), s) -> (f, (x, s))) >>> app)
    )

instance Arrow a => ArrowTransformer (ScheduleArr t) a where
  lift f = ScheduleArr (StateArrow (first f))

runScheduleArr
  :: Arrow a => ScheduleArr t a i o -> a (i, Schedule t) (o, Schedule t)
runScheduleArr = runState . unScheduleArr

state :: Arrow a => ((i, Schedule t) -> (o, Schedule t)) -> ScheduleArr t a i o
state = ScheduleArr . StateArrow . arr

-- | Run a schedule action like 'after', 'cancel', or 'renew'.
schedule
  :: Arrow a => (i -> Schedule t -> (o, Schedule t)) -> ScheduleArr t a i o
schedule = state . uncurry

-- | Run a schedule modification like 'acquireLiveTask' or 'releaseLiveTask'.
schedule' :: Arrow a => (i -> Schedule t -> Schedule t) -> ScheduleArr t a i ()
schedule' = state . (((), ) .) . uncurry

-- | Get a schedule property like 'tickNow', 'tickPrev', or 'ticksToIdle'.
getSched :: Arrow a => (Schedule t -> o) -> ScheduleArr t a i o
getSched f = ScheduleArr fetch >>> arr f

-- TODO: export to upstream arrows or extra
whileJustA :: (ArrowChoice a, Monoid o) => a i (Maybe o) -> a i o
whileJustA act = arr (, mempty) >>> go
 where
  go = proc (i, rr) -> do
    r' <- act -< i
    case r' of
      Nothing -> returnA -< rr
      Just r  -> go -< (i, rr <> r)

runTick
  :: (ArrowChoice a, Monoid o)
  => ScheduleArr t a (Tick, t) o
  -> ScheduleArr t a Tick o
runTick runTickTask = whileJustA $ proc tick -> do
  r' <- schedule (const popOrTick) -< ()
  case r' of
    Nothing     -> returnA -< Nothing
    Just (c, t) -> do
      () <- schedule' acquireLiveTask -< c
      r  <- runTickTask -< (tick, t) -- TODO: catch Haskell exceptions here
      () <- schedule' releaseLiveTask -< c
      returnA -< Just r

runTicksTo
  :: (ArrowChoice a, Monoid o)
  => ScheduleArr t a (Tick, t) o
  -> ScheduleArr t a Tick o
runTicksTo runTask = whileJustA $ proc tick -> do
  tick' <- getSched tickNow -< ()
  if tick' >= tick
    then returnA -< Nothing
    else arr Just <<< runTick runTask -< tick

getInput
  :: (Arrow a)
  => a TickDelta (Either Tick i)
  -> ScheduleArr t a i' (Either Tick i)
getInput getTimedInput =
  getSched ticksToIdle >>> arr (fromMaybe maxBound) >>> lift getTimedInput

mkOutput
  :: (ArrowChoice a, Monoid o)
  => ScheduleArr t a (Tick, t) o
  -> ScheduleArr t a i o
  -> ScheduleArr t a (Either Tick i) o
mkOutput runTask runInput = runTicksTo runTask ||| runInput
