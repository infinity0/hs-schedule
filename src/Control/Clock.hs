{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

{-| Pure abstractions for time and clocks. -}

module Control.Clock
  ( Clock(..)
  , clockTick
  , Clocked(..)
  , module Data.Schedule
  )
where

-- internal
import           Data.Schedule (Tick, TickDelta)

{-| A maybe-impure supplier of time, to a pure scheduled computation.

The type @c@ is the computational context where clock operations occur, e.g. a
'Monad' such as 'IO'.

Clock implementations /must/ be monotonic. See "System.Time.Monotonic" for an
example on how to wrap non-monotonic clocks to be monotonic.
-}
class Clock m c where
  -- | Get the current time.
  clockNow :: c -> m Tick

  {-| Suspend the current computation for a given number of ticks.

  Nothing else in the computation runs until the suspension is over.
  Afterwards, 'clockNow' will give the expected value, i.e. for all @n@:

  > do
  >     old <- clockNow
  >     clockDelay n
  >     new <- clockNow
  >     let new' = assert (old + n <= new) new

  The relation is '<=' not '==', because the computer might have slept during
  the mean time or something. On the other hand, if the underlying physical
  clock might delay for a shorter period than requested, then implementations
  of this function /must/ loop-delay until the '<=' condition is satisfied.

  The above is the only condition that scheduled computations should rely on,
  and any actual physical real delay is up to the implementation.
  -}
  clockDelay :: c -> TickDelta -> m ()

  {-| Interleave actions with ticks.

  This is typically recommended for the use-case where your action represents
  a stream of inputs, e.g. from the network or the user. It is meant to
  satisfy the same functionality as the @select@ system call found in common
  operating systems, used with a timeout parameter.

  If @action@ when executed repeatedly gives a sequence of results, with
  'Nothing' meaning EOF, then in the expression @clkAct <- 'clockWith' clock
  action@, a subsequent call to @'runClocked' clkAct@ when executed repeatedly
  gives the same sequence of results but with ticks interleaved in between
  them. Executing @'finClocked' clkAct@ closes any resources and invalidates
  any future calls to @clkAct@.

  If 'runClocked' runs in an exception-capable monad such as 'IO' then one
  should run 'finClocked' as part of a cleanup phase using something like
  @finally@ - there are different versions, for example:

  - t'Control.Exception.finally' from "Control.Exception"
  - t'Control.Monad.Catch.finally' from "Control.Monad.Catch"
  - t'UnliftIO.Exception.finally' from "UnliftIO.Exception"

  You should understand their differences before choosing one.
  -}
  clockWith :: c -> m (Maybe a) -> m (Clocked m a)

  {-| Given an action, run it with a timeout.

  This is typically recommended for the use-case where your action represents
  the response to a single previously-sent request.

  The action may complete despite the timeout firing, in which case its
  result will be lost. This is in general unavoidable and is a common
  property that one simply has to live with in distributed systems. If you
  run the input action repeatedly, then this property applies *for every
  execution*, i.e. it is possible that you get 10 timeouts even though the
  action succeeded 10 times, and you'll lose 10 results.

  If you want all results of all actions, use @clockWith@ instead. The
  downside with that, is that it's slightly less efficient than this, as it
  will interleave every single 'Tick' event and it is up to you to deal with
  skipping/ignoring any of them.
  -}
  clockTimer :: c -> TickDelta -> m a -> m (Either Tick a)

-- | Run 'clockDelay' then 'clockNow'.
clockTick :: (Clock m c, Monad m) => c -> TickDelta -> m Tick
clockTick clock d = clockDelay clock d >> clockNow clock

-- | See 'clockWith' for details on what this is for.
data Clocked m a = Clocked
  { runClocked :: !(m (Maybe (Either Tick a)))
  , finClocked :: !(m ())
  }
