{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

{-| Data structure representing scheduled tasks.

Most of the time you will want the more fully-featured "Control.Monad.Schedule"
or "Control.Arrow.Schedule" modules instead, which re-export this module.
-}
module Data.Schedule
  ( Tick
  , TickDelta
  , Task
  , TaskStatus(..)
  , Schedule
  , newSchedule
  , checkValidity
  , tickNow
  , tickPrev
  , ticksToIdle
  , taskStatus
  , after
  , cancel
  , cancel_
  , renew
  -- | = Other general utilities
  -- General monad / state-transition utils to be exported to another library
  , whileJustM
  , modST
  , getST
  , stA
  , imodA
  , getA
  )
where

import           Data.Schedule.Internal


-- | Run an action, accumulating its monoid result until it returns @Nothing@.
--
-- TODO: export to upstream extra
whileJustM :: (Monad m, Monoid a) => m (Maybe a) -> m a
whileJustM act = go mempty
 where
  go accum = act >>= \case
    Just r  -> go (accum <> r)
    Nothing -> pure accum

-- | Convert a modification function into a state transition function.
modST :: (s -> s) -> (s -> ((), s))
modST f s = ((), f s)
{-# INLINE modST #-}

-- | Convert a getter function into a state transition function.
getST :: (s -> o) -> (s -> (o, s))
getST f s = (f s, s)
{-# INLINE getST #-}

-- | Convert a state transition function into a state transition arrow.
stA :: (s -> os) -> ((i, s) -> os)
stA f = f . snd
{-# INLINE stA #-}

-- | Convert a modification arrow into a state transition arrow.
imodA :: (i -> s -> s) -> ((i, s) -> ((), s))
imodA f = ((), ) . uncurry f
{-# INLINE imodA #-}

-- | Convert a getter function into a state transition arrow.
getA :: (s -> a) -> ((i, s) -> (a, s))
getA f = getST f . snd
{-# INLINE getA #-}
