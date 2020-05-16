{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Control.Static.ScheduleTest where

-- external
import           Test.Tasty                       hiding (after)
import           Test.Tasty.HUnit

import           Control.Lens                     (Lens', Optic', Prism',
                                                   review, view, zoom, (^?),
                                                   _Wrapped)
import           Control.Lens.TH                  (makeWrapped)
import           Control.Monad                    (when)
import           Control.Monad.Trans.Class        (MonadTrans (lift))
import           Control.Monad.Trans.Maybe        (MaybeT (MaybeT, runMaybeT))
import           Control.Monad.Trans.Reader       (ReaderT (..))
import           Control.Monad.Trans.State.Strict (StateT (..), state)
import           Control.Static                   (ClosureApply, NullC2Sym0,
                                                   RepVal (..), TCTab (..),
                                                   applyClosure,
                                                   evalSomeClosure,
                                                   mkClosureTab, repClosureTab)
import           Control.Static.TH                (mkDefStaticTab,
                                                   mkStaticsWithRefs, staticKey,
                                                   staticRef)
import           Data.Primitive.MutVar            (newMutVar, readMutVar)

-- internal
import           Control.Clock.IO
import           Control.Monad.Primitive.Extra
import           Control.Monad.Schedule
import           Data.Rsv.RMMap                   (RMMap (..), empty)
import           Data.Schedule.Internal


data SomeArg = CountdownArg !TArg deriving (Eq, Ord, Show)
instance RepVal SomeArg TArg k where
  toRep _ = CountdownArg
  fromRep _ (CountdownArg i) = Right i

data TEnv m t = TEnv {
    cRunSched  :: !(RunSched t m)
  , cTaskPrism :: !(Prism' t (ClosureApply SomeArg))
  }
type TArg = Tick
type TRes = [Tick]
type C tm m = (MonadTrans tm, Monad (tm m))

mkStaticsWithRefs $ \(~[countdown']) -> [d|
  countdown :: C tm IO => TEnv (tm IO) t -> TArg -> tm IO TRes
  countdown env x = do
    when (x > 0) $ do
      n <- cRunSched $ getST $ tickNow
      let task = applyClosure $(pure countdown') (pred x)
      t <- cRunSched $ after 1 $ mk task
      s <- cRunSched $ getST $ taskStatus t
      lift $ assertEqual "task status is pending after 'after'"
                         ((^? cTaskPrism) <$> s)
                         (TaskPending (n + 1) (Just task))
    pure [x]
   where
    TEnv {..} = env
    mk        = review cTaskPrism
  |]
mkDefStaticTab ['countdown]

smoke
  :: (C tm IO, MonadFail (tm IO), Eq t, Show t)
  => (Clock IO -> IO (TickDelta -> IO (Either Tick i)))
  -> (Schedule t -> tm IO TRes -> IO (TRes, Schedule t))
  -> TEnv (tm IO) t
  -> IO ()
smoke mkRecv runWithNew env = do
  clock <- newClock' (interval 1 Ms)
  recv  <- mkRecv clock
  let top = 17
  (r, s) <- runWithNew newSchedule $ do
    _ <- cRunSched $ after 1 $ do
      mk $ applyClosure $(staticRef 'countdown) top
    whileJustM $ runMaybeT $ do
      MaybeT (cRunSched $ getST $ ticksToIdle) >>= \d -> lift $ do
        lift (recv d) >>= mkOutput cRunSched run undefined
  assertEqual "results" [top, top - 1 .. 0] r
  assertBool "schedule.now" $ now s > top
  assertEqual "schedule.tasks" (empty { handles = handles (tasks s) }) (tasks s)
  assertEqual "schedule.*" (newSchedule { now = now s, tasks = tasks s }) s
  assertEqual "schedule valid" (checkValidity s) Nothing
 where
  TEnv {..} = env
  mk        = review cTaskPrism
  run _ task = case task ^? cTaskPrism of
    Nothing -> pure []
    Just cl -> case evalSomeClosure closureTab cl of
      Left  e -> fail (show e)
      Right r -> r

  envTab     = TCCons $(staticKey 'countdown) env $ TCNil @NullC2Sym0
  closureTab = repClosureTab $ mkClosureTab staticTab envTab

-- Note: it is important that the env-type e / state-type s do not depend on
-- the task-type t, otherwise they will become mutually-recursive which is
-- impossible to achieve in Haskell.

runSchedMV
  :: PrimMonad m => Lens' e (PrimST m (Schedule t)) -> RunSched t (ReaderT e m)
runSchedMV lens sched = view lens >>= \run -> lift (statePrimST run sched)

newtype TaskMV m = TaskMV (ClosureApply SomeArg) deriving (Eq, Ord, Show)
makeWrapped ''TaskMV
type SchedMV m = PrimST m (Schedule (TaskMV m))

runSchedST :: Monad m => Lens' s (Schedule t) -> RunSched t (StateT s m)
runSchedST lens = zoom lens . state

newtype TaskST m = TaskST (ClosureApply SomeArg) deriving (Eq, Ord, Show)
makeWrapped ''TaskST
type SchedST m = Schedule (TaskST m)

type SOptic a = forall f . Optic' (->) f a a

tests :: TestTree
tests = testGroup
  "Control.Static.ScheduleTest"
  [ testCase "smoke clockTimer" $ do
    smoke (\clock -> pure (flip (clockTimer clock) voidInput))
          (flip runStateT)
          (TEnv (runSchedST (id :: SOptic (SchedST IO))) _Wrapped)
  , testCase "smoke clockWith" $ do
    -- TODO: we should call 'fin' (see clockWith) after the test but meh
    smoke
      (\clock -> const . runClocked <$> clockWith clock voidInput)
      (\s0 act -> do
        mv <- newMutVar s0
        r  <- runReaderT act (stMutVar mv)
        s1 <- readMutVar mv
        pure (r, s1)
      )
      (TEnv (runSchedMV (id :: SOptic (SchedMV IO))) _Wrapped)
  ]
