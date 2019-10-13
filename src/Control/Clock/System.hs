{-# LANGUAGE RankNTypes #-}

{-| Implementation of a 'Clock' based on the system monotonic clock. -}

module Control.Clock.System
  ( newClock
  , newClock1ms
  , newClock1s
  , convClock
  , timerFromIOClock
  , voidInput
  )
where

-- external
import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.Async       ( race )
import           Control.Monad                  ( forever )
import           Data.Void                      ( Void )

import qualified System.Time.Monotonic         as T

-- internal
import           Control.Clock
import           Data.Time.Clock


-- | Create a new clock ticking at a given rate.
newClock :: DiffTime -> IO (Clock IO)
newClock intv = convClock intv <$> T.newClock

-- | Create a new clock ticking at 1 millisecond.
newClock1ms :: IO (Clock IO)
newClock1ms = convClock (picosecondsToDiffTime 1000000000) <$> T.newClock

-- | Create a new clock ticking at 1 second.
newClock1s :: IO (Clock IO)
newClock1s = convClock (picosecondsToDiffTime 1000000000000) <$> T.newClock

-- | Check for a non-negative number.
checkNonNeg :: (Num a, Ord a, Show a) => a -> a
checkNonNeg n =
  if n >= 0 then n else error $ "must be non-negative: " ++ show n

-- | Check for a positive number.
checkPos :: (Num a, Ord a, Show a) => a -> a
checkPos n = if n > 0 then n else error $ "must be positive: " ++ show n

{-| Convert a system monotonic 'System.Time.Monotonic.Clock' into an abstract
    'Clock' for scheduled computations, ticking at the given rate.

    The first argument is the length of each tick.
-}
convClock :: DiffTime -> T.Clock -> Clock IO
convClock rate c =
  let r = diffTimeToPicoseconds $ checkPos rate
  in  Clock
        { clockNow   = (`div` r) . diffTimeToPicoseconds <$> T.clockGetTime c
        , clockDelay = T.delay
                       . picosecondsToDiffTime
                       . (r *)
                       . fromIntegral
                       . checkNonNeg
        }

-- TODO: is race actually safe e.g. when reading from a pipe?
timerFromIOClock :: Clock IO -> IO a -> TickDelta -> IO (Either Tick a)
timerFromIOClock c input d = race (clockTick c d) input

voidInput :: IO Void
voidInput = forever $ threadDelay maxBound
