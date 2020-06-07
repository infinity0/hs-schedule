{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

{-| Implementations of 'Clock' in the 'IO' monad. -}
module Control.Clock.IO
  ( Intv(..)
  , interval
  , IOClock(..)
  , convClock
  , newClock
  , newClock'
  , clockWithIO
  , clockTimerIO
  , voidInput
  , module Control.Clock
  )
where

-- external
import qualified System.Time.Monotonic          as T

import           Control.Concurrent             (threadDelay)
import           Control.Concurrent.Async       (async, asyncThreadId, cancel,
                                                 link, link2, race)
import           Control.Concurrent.STM         (STM, atomically, orElse)
import           Control.Concurrent.STM.TBQueue (TBQueue, isEmptyTBQueue,
                                                 newTBQueueIO, readTBQueue,
                                                 tryPeekTBQueue, tryReadTBQueue,
                                                 writeTBQueue)
import           Control.Exception              (AsyncException (..),
                                                 handleJust, throwTo)
import           Control.Monad                  (forever, unless, when)
import           Data.Schedule                  (untilJustM)
import           Data.Time.Clock                (DiffTime,
                                                 diffTimeToPicoseconds,
                                                 picosecondsToDiffTime)
import           Data.Void                      (Void)
import           GHC.Stack                      (HasCallStack)

-- internal
import           Control.Clock


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

newClock' :: DiffTime -> IO IOClock
newClock' = newClock 0

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

  clockWith c = clockWithIO c
  clockTimer c = clockTimerIO c

-- assert that a writeTBQueue is non-blocking
writeTBQueue' :: HasCallStack => TBQueue a -> a -> STM ()
writeTBQueue' q r = do
  e <- isEmptyTBQueue q
  unless e $ error "failed to assert non-blocking write on TBQueue"
  writeTBQueue q r

isUserInterrupt :: AsyncException -> Maybe AsyncException
isUserInterrupt UserInterrupt = Just UserInterrupt
isUserInterrupt _             = Nothing

-- | Interleave an action with clock ticks.
clockWithIO :: IOClock -> IO a -> IO (Clocked IO a)
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

  let fin = cancel actionThread >> cancel tickThread
      rethrow e = throwTo (asyncThreadId actionThread) e >> pure Nothing
      -- we don't expect UserInterrupt in non-interactive mode anyways, so
      -- just leave the below in for simplicity
      action' = untilJustM $ handleJust isUserInterrupt rethrow $ do
        atomically $ tryPeekTBQueue qi >>= \case
          Nothing -> writeTBQueue qi ()
          Just () -> pure ()
        atomically $ fmap Just $ do
          (Right <$> readTBQueue qo) `orElse` (Left <$> readTBQueue qt)

  pure (Clocked action' fin)

clockTimerIO :: IOClock -> TickDelta -> IO a -> IO (Either Tick a)
clockTimerIO c d = race (clockTick c d)

voidInput :: IO Void
voidInput = forever $ threadDelay maxBound
