{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

{-| Implementations of 'Clock' in the 'IO' monad. -}
module Control.Clock.IO
  ( Intv(..)
  , interval
  , IOClock(..)
  , convClock
  , newClock
  , newClockSystem
  , clockWithIO
  , clockWithIOs
  , clockTimerIO
  , voidInput
  , module Control.Clock
  )
where

-- external
import qualified System.Time.Monotonic     as T

import           Control.Concurrent        (threadDelay)
import           Control.Concurrent.Async  (race)
import           Control.Monad             (forever, when)
import           Data.Time.Clock           (DiffTime, diffTimeToPicoseconds,
                                            nominalDiffTimeToSeconds,
                                            picosecondsToDiffTime)
import           Data.Time.Clock.POSIX     (getPOSIXTime)
import           Data.Void                 (Void)
import           GHC.Stack                 (HasCallStack)

-- internal
import           Control.Clock
import           Control.Clock.IO.Internal


data Intv = Ps | Ns | Us | Ms | S

-- | Convenience method for creating @DiffTime@ for use with @newClock@.
interval :: Integer -> Intv -> DiffTime
interval i u = picosecondsToDiffTime $ case u of
  Ps -> i
  Ns -> 1000 * i
  Us -> 1000000 * i
  Ms -> 1000000000 * i
  S  -> 1000000000000 * i

-- | Check for a non-negative number.
checkNonNeg :: (HasCallStack, Num a, Ord a, Show a) => a -> a
checkNonNeg n =
  if n >= 0 then n else error $ "must be non-negative: " ++ show n

-- | Check for a positive number.
checkPos :: (HasCallStack, Num a, Ord a, Show a) => a -> a
checkPos n = if n > 0 then n else error $ "must be positive: " ++ show n

clockNowPico :: Tick -> T.Clock -> IO Integer
clockNowPico start c = (start +) . diffTimeToPicoseconds <$> T.clockGetTime c

clockDelayPico :: Integer -> IO ()
clockDelayPico d = T.delay $ picosecondsToDiffTime $ checkNonNeg d

data IOClock = IOClock
  { cStart :: Tick
  , cRate  :: Integer
  , cClock :: T.Clock
  }

{-| Convert a "System.Time.Monotonic.Clock" into an abstract 'Clock' for
    scheduled computations, ticking at the given interval.
-}
convClock :: Tick -> DiffTime -> T.Clock -> IOClock
convClock start intv c =
  let r = diffTimeToPicoseconds $ checkPos intv in IOClock start r c

-- | Create a new clock with the given start tick and interval.
newClock :: Tick -> DiffTime -> IO IOClock
newClock start intv = convClock start intv <$> T.newClock

{- | Create a new clock with the given interval.

The start tick is set non-deterministically by the runtime system, and is
approximately the current UNIX timestamp multiplied by the interval in
milliseconds; however this is not a guarantee and should not be relied upon.

Use 'newClock' for more predictable behaviour for e.g. testing and replays.
-}
newClockSystem :: DiffTime -> IO IOClock
newClockSystem intv = do
  let intvMs = diffTimeToPicoseconds intv `div` 1000000000
  now <- nominalDiffTimeToSeconds <$> getPOSIXTime
  newClock (floor now * intvMs) intv

instance Clock IO IOClock where
  clockNow (IOClock start r c) =
    let i = start * r in (`div` r) <$> clockNowPico i c
  clockDelay (IOClock start r c) d =
    let i = start * r
    in  when (d > 0) $ do
          remain <- (`rem` r) <$> clockNowPico i c
          -- wait a bit past the tick, make sure we've gone over
          let t = r * fromIntegral d * 16 `div` 15 - remain
          clockDelayPico t

  clockWith  = clockWithIO
  clockTimer = clockTimerIO

-- | Interleave an action with clock ticks.
clockWithIO :: IOClock -> IO (Maybe a) -> IO (Clocked IO a)
clockWithIO clock action = clockWithIOs clock [action]

-- | Interleave several actions together with clock ticks.
clockWithIOs :: IOClock -> [IO (Maybe a)] -> IO (Clocked IO a)
clockWithIOs clock actions = do
  let clock'   = Just . Left <$> clockTick clock 1
      actions' = ((Right <$>) <$>) <$> actions
  (act, fin) <- foreverInterleave (const (pure True)) (clock' : actions')
  pure (Clocked act fin)

clockTimerIO :: IOClock -> TickDelta -> IO a -> IO (Either Tick a)
clockTimerIO c d = race (clockTick c d)

voidInput :: IO Void
voidInput = forever $ threadDelay maxBound
