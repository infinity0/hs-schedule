{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.Trans.ScheduleTest where

-- external
import           Test.Tasty                   hiding (after)
import           Test.Tasty.HUnit

import           Control.Monad                (void, when)
import           Control.Monad.Trans.Class    (MonadTrans (lift))
import           Control.Monad.Trans.Maybe    (MaybeT (MaybeT, runMaybeT))

-- internal
import           Control.Clock.System
import           Control.Monad.Trans.Schedule
import           Data.Rsv.RMMap
import           Data.Schedule.Internal


tests :: TestTree
tests = testGroup
  "Control.Monad.Trans.ScheduleTest"
  [ testCase "smoke clockTimer"
    $ smoke (\clock -> pure (flip (clockTimer clock) voidInput))
  , testCase "smoke clockWith"
    -- TODO: we should call 'fin' (see clockWith) after the test but meh
    $ smoke (\clock -> const . runClocked <$> clockWith clock voidInput)
  ]

smoke :: (Clock IO -> IO (TickDelta -> IO (Either Tick i))) -> IO ()
smoke mkRecv = do
  clock <- newClock1ms
  recv  <- mkRecv clock
  let top = 17
  (r, s) <- flip runScheduleT newSchedule $ do
    _ <- schedule $ after 1 top
    whileJustM $ runMaybeT $ do
      MaybeT (getSched ticksToIdle) >>= \d -> lift $ do
        lift (recv d) >>= mkOutput countdown undefined
  assertEqual "results" [top, top - 1 .. 0] r
  assertBool "schedule.now" $ now s > top
  assertEqual "schedule.tasks" (empty { handles = handles (tasks s) }) (tasks s)
  assertEqual "schedule.*" (newSchedule { now = now s, tasks = tasks s }) s
 where
  countdown _ x = do
    when (x > 0) $ do
      void $ schedule $ after 1 $ pred x
    pure [x]
