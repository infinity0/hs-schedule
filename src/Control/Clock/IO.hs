{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

{-| Implementations of 'Clock' in the 'IO' monad. -}
module Control.Clock.IO
  ( newClock
  , newClock'
  , Intv(..)
  , interval
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


-- | Create a new clock with the given start tick and interval.
newClock :: Tick -> DiffTime -> IO (Clock IO)
newClock start intv = convClock start intv <$> T.newClock

newClock' :: DiffTime -> IO (Clock IO)
newClock' = newClock 0

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

{-| Convert a "System.Time.Monotonic.Clock" into an abstract 'Clock' for
    scheduled computations, ticking at the given interval.
-}
convClock :: Tick -> DiffTime -> T.Clock -> Clock IO
convClock start intv c =
  let r  = diffTimeToPicoseconds $ checkPos intv
      i  = start * r
      c' = Clock
        { clockNow   = (`div` r) <$> clockNowPico i c
        , clockDelay = \d -> when (d > 0) $ do
                         remain <- (`rem` r) <$> clockNowPico i c
                         -- wait a bit past the tick, make sure we've gone over
                         let t = r * fromIntegral d * 16 `div` 15 - remain
                         clockDelayPico t
        , clockWith  = clockWithIO c'
        , clockTimer = clockTimerIO c'
        }
  in  c'

clockNowPico :: Tick -> T.Clock -> IO Integer
clockNowPico start c = (start +) . diffTimeToPicoseconds <$> T.clockGetTime c

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
      writeTBQueue' qi ()
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
