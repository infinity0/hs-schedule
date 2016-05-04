{-# LANGUAGE RankNTypes #-}

{-| Run a pure scheduled computation impurely via 'MonadIO' -}

module Control.Monad.Trans.Schedule.IO (
    ioLiftClock
  , ioLiftRT'
  , ioLiftRT
  ) where

-- external
import Control.Concurrent.Async (Async(..), async, poll, wait)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(lift))

-- ours
import Control.Clock (Clock(..))
import Control.Monad.Trans.Schedule.Internal (ScheduleT, getClock, runTasksTo, LiftClock, LiftRT)


-- | Lift a IO clock computation.
ioLiftClock :: MonadIO m => LiftClock IO m
ioLiftClock = liftIO

{-| Run an 'IO' action and run tasks /while/ it is blocked.
    You may use this with:

    * 'ioLiftClock', if your scheduled computation wraps a 'MonadIO'; or
    * 'Control.Monad.Trans.Schedule.Base.baseLiftClock', if it wraps a
      @'Control.Monad.Base.MonadBase' 'IO'@.
-}
ioLiftRT' :: Monad m => LiftClock IO m -> LiftRT IO IO m
ioLiftRT' liftClockIO' io = do
    c <- getClock
    aio <- lift . liftClockIO' $ async io
    tickAsync' liftClockIO' c aio

tickAsync' :: Monad m => LiftClock IO m -> Clock IO -> Async a -> ScheduleT IO m a
tickAsync' liftClockIO' c aio = do
    let liftC = lift . liftClockIO'
    res <- liftC $ poll aio
    case res of
        Just _ -> liftC $ wait aio -- should return immediately
        Nothing -> do
            nextTick <- liftC $ clockDelay c 1 >> clockNow c
            runTasksTo nextTick
            tickAsync' liftClockIO' c aio

-- | Run an 'IO' action and run tasks /while/ it is blocked.
ioLiftRT :: MonadIO m => LiftRT IO IO m
ioLiftRT = ioLiftRT' ioLiftClock
