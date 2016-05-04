{-# LANGUAGE RankNTypes #-}

module Control.Monad.Trans.Schedule.Example where

import Control.Clock.System
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Morph
import Control.Monad.Trans.Schedule
import Control.Monad.Trans.State.Strict

{- | Basic logic example, with no impure dependencies. -}
mainPure :: Monad m
    => LiftRT c b (StateT Bool m)
    -> Tick
    -> b String
    -> ScheduleT c (StateT Bool m) String
mainPure liftRT thresholdTicks getString = do
    _ <- after thresholdTicks $ lift (put True)
    str <- liftRT getString
    slow <- lift get
    return $ if slow then "! too slow !" else "= " ++ str

{- | Basic usage example, for automated testing.

    >>> let delayMs = Control.Concurrent.threadDelay . (* 1000)
    >>> main' 25 $ delayMs 10 >> return "lol"
    = lol

    >>> main' 25 $ delayMs 50 >> return "lol"
    ! too slow !
-}
main' :: Tick -> IO String -> IO ()
main' thresholdMs input = do
    clock <- newClock
    let mainImpure :: ScheduleT IO (StateT Bool IO) ()
        mainImpure = mainPure ioLiftRT thresholdMs input >>= (lift . liftIO . putStrLn)
    void $ mainImpure `runScheduleT` clock `runStateT` False

{- | Basic usage example, for human interactive use. -}
main :: IO ()
main = main' 250 getLine
