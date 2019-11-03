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
  , renew
  )
where

import           Data.Schedule.Internal
