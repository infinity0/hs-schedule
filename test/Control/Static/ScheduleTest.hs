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

-- this is for a TH expression so not a runtime issue
-- sadly GHC doesn't let us disable this per-expression
-- https://gitlab.haskell.org/ghc/ghc/issues/18260
{-# OPTIONS_GHC -Wno-error=incomplete-uni-patterns #-}

module Control.Static.ScheduleTest where

-- external
import           Test.Tasty                       hiding (after)
import           Test.Tasty.HUnit

import           Control.Lens                     (Lens', Optic', Prism',
                                                   review, view, zoom, (^?))
import           Control.Lens.Mutable             (ASLens, AsLens (..),
                                                   MonadLST, PrimOpGroup (..),
                                                   runASLens)
import           Control.Monad                    (when)
import           Control.Monad.Trans.Class        (MonadTrans (lift))
import           Control.Monad.Trans.Maybe        (MaybeT (MaybeT, runMaybeT))
import           Control.Monad.Trans.Reader       (ReaderT (..))
import           Control.Monad.Trans.State.Strict (StateT (..), state)
import           Control.Static                   (ClosureApply, RepVal (..),
                                                   applyClosure, envTabCons,
                                                   envTabNil, evalSomeClosure,
                                                   mkClosureTab, repClosureTab)
import           Control.Static.TH                (mkDefStaticTab,
                                                   mkStaticsWithRefs, staticRef)
import           Data.Primitive.MutVar            (newMutVar, readMutVar)

-- internal
import           Control.Clock.IO
import           Control.Monad.Schedule
import           Data.Rsv.RMMap                   (RMMap (..), empty)
import           Data.Schedule.Internal


data SomeArg = CountdownArg !TArg
  deriving (Eq, Ord, Show)
instance RepVal SomeArg TArg k where
  toRep _ = CountdownArg
  fromRep _ (CountdownArg i) = Right i

data TEnv m t = TEnv
  { cRunSched  :: !(RunSched t m)
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

  envTab     = envTabCons $(staticRef 'countdown) env $ envTabNil
  closureTab = repClosureTab $ mkClosureTab staticTab envTab

-- Note: it is important that the env-type e / state-type s do not depend on
-- the task-type t, otherwise they will become mutually-recursive which is
-- impossible to achieve in Haskell.

runSchedM
  :: MonadLST p s m
  => Lens' e (ASLens p s (Schedule t))
  -> RunSched t (ReaderT e m)
runSchedM lens sched = view lens >>= \slens -> lift (runASLens slens sched)
type SchedM p s m = ASLens p s (Schedule (ClosureApply SomeArg))

runSchedS :: Monad m => Lens' s (Schedule t) -> RunSched t (StateT s m)
runSchedS lens = zoom lens . state
type SchedS m = Schedule (ClosureApply SomeArg)

type SOptic a = forall f . Optic' (->) f a a

tests :: TestTree
tests = testGroup
  "Control.Static.ScheduleTest"
  [ testCase "smoke clockTimer" $ do
    smoke (\clock -> pure (flip (clockTimer clock) voidInput))
          (flip runStateT)
          (TEnv (runSchedS (id :: SOptic (SchedS IO))) id)
  , testCase "smoke clockWith" $ do
    -- TODO: we should call 'fin' (see clockWith) after the test but meh
    smoke
      (\clock -> const . runClocked <$> clockWith clock voidInput)
      (\s0 act -> do
        mv <- newMutVar s0
        r  <- runReaderT act (asLens mv)
        s1 <- readMutVar mv
        pure (r, s1)
      )
      (TEnv (runSchedM (id :: SOptic (SchedM 'OpST s IO))) id)
  ]
