{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}

{-| Implementations of 'Clock' in the 'IO' monad. -}
module Control.Clock.IO
  ( newClock
  , newClockPico
  , newClock1ms
  , newClock1s
  , convClock
  , clockWithIO
  , clockTimerIO
  , voidInput
  , module Control.Clock
  )
where

-- external
import qualified System.Time.Monotonic          as T

import           Control.Concurrent             (threadDelay)
import           Control.Concurrent.Async       (async, cancel, link, link2,
                                                 race)
import           Control.Concurrent.STM         (STM, atomically, orElse)
import           Control.Concurrent.STM.TBQueue (TBQueue, isEmptyTBQueue,
                                                 newTBQueueIO, readTBQueue,
                                                 tryPeekTBQueue, tryReadTBQueue,
                                                 writeTBQueue)
import           Control.Monad                  (forever, unless, when)
import           Data.Time.Clock                (DiffTime,
                                                 diffTimeToPicoseconds,
                                                 picosecondsToDiffTime)
import           Data.Void                      (Void)
import           GHC.Stack                      (HasCallStack)

-- internal
import           Control.Clock


-- | Create a new clock ticking at a given interval.
newClock :: DiffTime -> IO (Clock IO)
newClock intv = convClock intv <$> T.newClock

-- | Create a new clock ticking at a given interval in picoseconds.
newClockPico :: Integer -> IO (Clock IO)
newClockPico = newClock . picosecondsToDiffTime

-- | Create a new clock ticking at 1 millisecond.
newClock1ms :: IO (Clock IO)
newClock1ms = newClockPico 1000000000

-- | Create a new clock ticking at 1 second.
newClock1s :: IO (Clock IO)
newClock1s = newClockPico 1000000000000

-- | Check for a non-negative number.
checkNonNeg :: (HasCallStack, Num a, Ord a, Show a) => a -> a
checkNonNeg n =
  if n >= 0 then n else error $ "must be non-negative: " ++ show n

-- | Check for a positive number.
checkPos :: (HasCallStack, Num a, Ord a, Show a) => a -> a
checkPos n = if n > 0 then n else error $ "must be positive: " ++ show n

{-| Convert a system monotonic "System.Time.Monotonic.Clock" into an abstract
    'Clock' for scheduled computations, ticking at the given interval.

    The first argument is the length of each tick.
-}
convClock :: DiffTime -> T.Clock -> Clock IO
convClock intv c =
  let r  = diffTimeToPicoseconds $ checkPos intv
      c' = Clock
        { clockNow   = (`div` r) <$> clockNowPico c
        , clockDelay = \d -> when (d > 0) $ do
                         remain <- (`rem` r) <$> clockNowPico c
                         -- wait a bit past the tick, make sure we've gone over
                         let t = r * fromIntegral d * 16 `div` 15 - remain
                         clockDelayPico t
        , clockWith  = clockWithIO c'
        , clockTimer = clockTimerIO c'
        }
  in  c'

clockNowPico :: T.Clock -> IO Integer
clockNowPico c = diffTimeToPicoseconds <$> T.clockGetTime c

clockDelayPico :: Integer -> IO ()
clockDelayPico d = T.delay $ picosecondsToDiffTime $ checkNonNeg d

-- assert that a writeTBQueue is non-blocking
writeTBQueue' :: HasCallStack => TBQueue a -> a -> STM ()
writeTBQueue' q r = do
  e <- isEmptyTBQueue q
  unless e $ error "failed to assert non-blocking write on TBQueue"
  writeTBQueue q r

clockWithIO :: Clock IO -> IO a -> IO (Clocked IO a)
clockWithIO clock action = do
  qi           <- newTBQueueIO 1
  qo           <- newTBQueueIO 1
  qt           <- newTBQueueIO 1

  -- keep running action
  actionThread <- async $ forever $ do
    -- block until we get a request to run action, but don't pop the queue
    atomically $ do
      readTBQueue qi
      writeTBQueue qi ()
    r <- action
    -- pop the queue after we write the result of action
    atomically $ do
      writeTBQueue' qo r
      readTBQueue qi
  link actionThread

  -- keep producing ticks
  tickThread <- async $ forever $ do
    t <- clockTick clock 1
    atomically $ do
      _ <- tryReadTBQueue qt -- empty the queue before we write a tick
      writeTBQueue' qt t
  link tickThread

  -- Kill both threads if any one of them dies. This ensures that the user
  -- doesn't need to call fin themselves if anything throws an exception.
  link2 actionThread tickThread

  let fin     = cancel actionThread >> cancel tickThread
      action' = do
        atomically $ tryPeekTBQueue qi >>= \case
          Nothing -> writeTBQueue qi ()
          Just () -> pure ()
        atomically $ do
          (Right <$> readTBQueue qo) `orElse` (Left <$> readTBQueue qt)

  pure (Clocked action' fin)

clockTimerIO :: Clock IO -> TickDelta -> IO a -> IO (Either Tick a)
clockTimerIO c d = race (clockTick c d)

voidInput :: IO Void
voidInput = forever $ threadDelay maxBound
