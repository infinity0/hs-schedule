{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
  )
where

import           Data.Schedule.Internal
