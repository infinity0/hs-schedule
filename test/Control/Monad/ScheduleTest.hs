{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Control.Monad.ScheduleTest where

-- external
import           Test.Tasty                       hiding (after)
import           Test.Tasty.HUnit

import           Control.Lens.Mutable             (AsLens (..), MonadLST,
                                                   PrimOpGroup (..), S (..),
                                                   runSLens)
import           Control.Monad                    (when)
import           Control.Monad.Extra              (whileJustM)
import           Control.Monad.Trans.Class        (MonadTrans (lift))
import           Control.Monad.Trans.Maybe        (MaybeT (MaybeT, runMaybeT))
import           Control.Monad.Trans.Reader       (ReaderT (..), ask)
import           Control.Monad.Trans.State.Strict (StateT (..), state)
import           Data.Maybe                       (fromJust)
import           Data.Primitive.MutVar            (newMutVar, readMutVar)

-- internal
import           Control.Clock.IO
import           Control.Monad.Schedule
import           Data.Rsv.RMMap                   (isEmpty)
import           Data.Schedule.Internal


newtype TTask = TTask Tick deriving (Eq, Ord, Read, Show)

countdown
  :: (MonadTrans tm, Monad (tm IO))
  => RunSched TTask (tm IO)
  -> Tick
  -> TTask
  -> tm IO [Tick]
countdown runSched _ (TTask x) = do
  when (x > 0) $ do
    n <- runSched $ getST $ tickNow
    t <- runSched $ after 1 $ TTask $ pred x
    s <- runSched $ getST $ taskStatus t
    lift $ assertEqual "task status is pending after 'after'"
                       s
                       (TaskPending (n + 1) (TTask (pred x)))
  pure [x]

smoke
  :: (MonadTrans tm, Monad (tm IO))
  => (IOClock -> IO (TickDelta -> IO (Either Tick i)))
  -> (Schedule t -> tm IO [Tick] -> IO ([Tick], Schedule TTask))
  -> RunSched TTask (tm IO)
  -> IO ()
smoke mkRecv runWithNew runSched = do
  clock <- newClock' (interval 1 Ms)
  recv  <- mkRecv clock
  let top = 17
  (r, s) <- runWithNew newSchedule $ do
    _ <- runSched $ after 1 $ TTask top
    whileJustM $ runMaybeT $ do
      MaybeT (runSched $ getST $ ticksToIdle) >>= \d -> lift $ do
        lift (recv d) >>= mkOutput runSched (countdown runSched) undefined
  assertEqual "results" [top, top - 1 .. 0] r
  assertBool "schedule.now" $ now s > top
  assertBool "schedule.tasks" (isEmpty (tasks s))
  assertEqual "schedule.*" (newSchedule { now = now s, tasks = tasks s }) s
  assertEqual "schedule valid" (checkValidity s) Nothing

runSchedM
  :: forall p s m r t
   . MonadLST p s m
  => AsLens (S p s) (Schedule t) r => RunSched t (ReaderT (r (Schedule t)) m)
runSchedM sched = ask >>= \ref -> lift (runSLens @p @s (asLens ref) sched)

runSchedS :: Monad m => RunSched t (StateT (Schedule t) m)
runSchedS = state

tests :: TestTree
tests = testGroup
  "Control.Monad.ScheduleTest"
  [ testCase "smoke clockTimer" $ do
    smoke (\clock -> pure (flip (clockTimer clock) voidInput))
          (flip runStateT)
          runSchedS
  , testCase "smoke clockWith" $ do
    -- TODO: we should call 'fin' (see clockWith) after the test but meh
    smoke
      (\clock -> const . fmap fromJust . runClocked <$> clockWith
        clock
        (Just <$> voidInput)
      )
      (\s0 act -> do
        mv <- newMutVar s0
        r  <- runReaderT act mv
        s1 <- readMutVar mv
        pure (r, s1)
      )
      (runSchedM @ 'OpST)
  ]
