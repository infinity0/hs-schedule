{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-| Data structure representing scheduled tasks. -}

module Data.Schedule
  ( Tick
  , TickDelta
  , LiveTask
  , Schedule
  , newSchedule
  , ticksUntilNextTask
  , after
  , cancel
  , renew
  )
where

import           Data.Schedule.Internal
