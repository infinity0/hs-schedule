{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , StandaloneDeriving
  #-}

{-| Pure abstractions for time and clocks. -}

module Control.Clock (
    Tick
  , checkNonNeg
  , checkPos
  , tickOp
  , Clock(..)
  ) where

import Data.Monoid (Sum(..))

-- missing instances for Sum in Data.Monoid
-- TODO: upstream this
deriving instance Enum n => Enum (Sum n)
deriving instance Real n => Real (Sum n)
deriving instance Integral n => Integral (Sum n)
deriving instance Fractional n => Fractional (Sum n)

-- TODO: decide whether to change this to Int64
-- | The smallest discrete unit of time, in a pure scheduled computation.
type Tick = Sum Integer

-- | Check for a non-negative number.
checkNonNeg :: (Num a, Ord a, Show a) => a -> a
checkNonNeg n = if n >= 0 then n else error $ "must be non-negative: " ++ show n

-- | Check for a positive number.
checkPos :: (Num a, Ord a, Show a) => a -> a
checkPos n = if n > 0 then n else error $ "must be positive: " ++ show n

-- | Apply a binary operation to two real numbers, returning a Tick.
tickOp :: (Real a, Real b) => (Rational -> Rational -> Rational) -> a -> b -> Tick
tickOp op a b = Sum $ floor $ toRational a `op` toRational b

{-| A maybe-impure supplier of time, to a pure scheduled computation.

    The type 'c' is the computational context where clock operations occur,
    e.g. a 'Monad' such as 'IO'.

    Clock implementations /must/ be monotic. See "System.Time.Monotonic" for an
    example on how to wrap non-monotonic clocks. TODO: provide a generic
    monotonic wrapper.
-}
data Clock c = Clock {
    -- | Get the current time.
    clockNow :: c Tick
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
  , clockDelay :: Tick -> c ()
    -- Estimate the real time a tick is supposed to take.
    -- TODO: decide whether this is a good idea; it would add a depedency on [time]
    -- and also make it tempting to change a computation's behaviour based on this
    -- tickInterval :: Maybe DiffTime
}
