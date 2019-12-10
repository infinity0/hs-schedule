{-# LANGUAGE LambdaCase #-}

{-| Data structure representing scheduled tasks. -}

module Data.Schedule
  ( Tick
  , TickDelta
  , LiveTask
  , Schedule
  , newSchedule
  , tickNow
  , tickPrev
  , ticksToIdle
  , after
  , cancel
  , cancel_
  , renew
  -- general monad / ST utils to be exported to some other library
  , whileJustM
  , modST
  , getST
  )
where

import           Data.Schedule.Internal

-- TODO: export to upstream extra
whileJustM :: (Monad m, Monoid a) => m (Maybe a) -> m a
whileJustM act = go mempty
 where
  go accum = act >>= \case
    Just r  -> go (accum <> r)
    Nothing -> pure accum

-- | Convert a modification function into a state transition function.
modST :: (Schedule t -> Schedule t) -> (Schedule t -> ((), Schedule t))
modST f s = ((), f s)

-- | Convert a getter function into a state transition function.
getST :: (Schedule t -> a) -> (Schedule t -> (a, Schedule t))
getST f s = (f s, s)
