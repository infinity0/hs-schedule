{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-| Pure abstractions for time and clocks. -}

module Control.Clock
  ( Clock(..)
  , clockTick
  , module Data.Schedule
  )
where

-- internal
import           Data.Schedule                  ( Tick
                                                , TickDelta
                                                )

{-| A maybe-impure supplier of time, to a pure scheduled computation.

    The type 'c' is the computational context where clock operations occur,
    e.g. a 'Monad' such as 'IO'.

    Clock implementations /must/ be monotic. See "System.Time.Monotonic" for an
    example on how to wrap non-monotonic clocks. TODO: provide a generic
    monotonic wrapper.
-}
data Clock c = Clock {
    -- | Get the current time.
    clockNow   :: c Tick
    {-| Suspend the current computation for a given number of ticks.

        Nothing else in the computation runs until the suspension is over.
        Afterwards, 'clockNow' will give the expected value, i.e. for all @n@:

        > do
        >     old <- clockNow
        >     clockDelay n
        >     new <- clockNow
        >     let new' = assert (old + n <= new) new

        The relation is '<=' not '==', because the computer might have slept
        during the mean time or something. On the other hand, if the underlying
        physical clock might delay for a shorter period than requested, then
        implementations of this function /must/ loop-delay until the '<='
        condition is satisfied.

        The above is the only condition that scheduled computations should rely
        on, and any actual physical real delay is up to the implementation.
     -}
  , clockDelay :: TickDelta -> c ()
}

clockTick :: Monad c => Clock c -> TickDelta -> c Tick
clockTick clock d = clockDelay clock d >> clockNow clock
