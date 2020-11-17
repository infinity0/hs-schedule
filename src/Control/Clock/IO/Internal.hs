{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module Control.Clock.IO.Internal where

import           Control.Concurrent.Async       (AsyncCancelled (..), async,
                                                 cancel, pollSTM)
import           Control.Concurrent.STM         (atomically, retry)
import           Control.Concurrent.STM.TBQueue (newTBQueueIO, readTBQueue,
                                                 tryPeekTBQueue, writeTBQueue)
import           Control.Concurrent.STM.TVar    (newTVarIO, stateTVar)
import           Control.Exception              (Exception, SomeException,
                                                 fromException, mask, throwIO,
                                                 tryJust)
import           Control.Monad.Fix              (mfix)
import           Data.Foldable                  (for_)
import           Data.Function                  (on)
import           Data.List                      (minimumBy)
import           Data.Maybe                     (catMaybes, isNothing)
import           Data.Traversable               (for)


notCancel :: SomeException -> Maybe SomeException
notCancel e | Just AsyncCancelled <- fromException e = Nothing
            | otherwise                              = Just e

untry :: Exception e => Either e a -> IO a
untry = \case
  Left  e -> throwIO e
  Right r -> pure r

{- | Combine and interleave a bunch of repeated IO actions.

See https://github.com/simonmar/async/issues/113 for why this is needed.

TODO: upstream to e.g. extra or something
-}
foreverInterleave
  :: (Int -> IO Bool) -> [IO (Maybe a)] -> IO (IO (Maybe a), IO ())
foreverInterleave isEOF actions = do
  -- queue of results, to hold as they arrive
  -- this really should have capacity 0 not 1, but:
  -- https://github.com/haskell/stm/issues/28
  qos <- for actions (const (newTBQueueIO 1))
  -- counter to ensure arrival order
  c   <- newTVarIO (0 :: Int)

  ths <- mfix $ \ths -> for (zip3 [0 ..] actions qos) $ \(j, action, qo) -> do
    let write o = atomically $ do
          -- store the output with its arrival index
          c' <- stateTVar c (\i -> (i, succ i))
          writeTBQueue qo (c', o)
    let go restore = do
          -- keep running action, catching exceptions until cancelled
          out <- restore (tryJust notCancel action)
          case out of
            Right Nothing -> isEOF j >>= \case
              True -> do
                for_ ths cancel
              False -> do
                pure () -- stop looping
            Right (Just v) -> do
              write (Right v)
              go restore
            Left e -> do
              write (Left e)
              go restore
    async $ mask go

  let getElem = (traverse untry =<<) $ atomically $ do
        outs <- fmap catMaybes $ for qos $ \qo -> do
          fmap (, qo) <$> tryPeekTBQueue qo
        if not (null outs)
          then do
            -- if there are any elements left in queues, keep emitting them
            -- (even if all threads are dead)
            let ((_, out), qo) = minimumBy (compare `on` fst . fst) outs
            _ <- readTBQueue qo -- same as out
            pure (Just out)
          else do
            -- check if all threads dead, if so then quit (output Nothing)
            expects <- for ths $ fmap isNothing . pollSTM
            if True `notElem` expects
              then pure Nothing -- nothing expected, finished
              else retry -- stuff still active, retry

      cancelAll = for_ ths cancel

  pure (getElem, cancelAll)
