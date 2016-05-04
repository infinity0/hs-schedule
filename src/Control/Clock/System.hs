{-| Implementation of a 'Clock' based on the system monotonic clock. -}

module Control.Clock.System (
    newClock
  , picosecondsToDiffTime
  , newClockAt
  , convClock
  ) where

-- external
import Data.Time.Clock
import qualified System.Time.Monotonic as T

-- ours
import Control.Clock


-- | Apply a binary operation to two real numbers, returning a DiffTime.
diffTimeOp :: (Real a, Real b) => (Rational -> Rational -> Rational) -> a -> b -> DiffTime
diffTimeOp op a b = picosecondsToDiffTime $ floor $ 1000000000000 * (toRational a `op` toRational b)

-- | Create a new clock ticking at 1 millisecond.
newClock :: IO (Clock IO)
newClock = convClock (picosecondsToDiffTime 1000000000) <$> T.newClock

-- | Create a new clock ticking at a given rate.
newClockAt :: DiffTime -> IO (Clock IO)
newClockAt intv = convClock intv <$> T.newClock

{-| Convert a system monotonic 'System.Time.Monotonic.Clock' into an abstract
    'Clock' for scheduled computations, ticking at the given rate.

    The first argument is the length of each tick.
-}
convClock :: DiffTime -> T.Clock -> Clock IO
convClock rate c = let r = checkPos rate in Clock {
    clockNow = let div' = tickOp (/) in
        (`div'` rate) <$> T.clockGetTime c
  , clockDelay = let mul' = diffTimeOp (*) in
        T.delay . (`mul'` r) . checkNonNeg
}
