{-# LANGUAGE RankNTypes #-}

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

    The type @c@ is the computational context where clock operations occur,
    e.g. a 'Monad' such as 'IO'.

    Clock implementations /must/ be monotonic. See "System.Time.Monotonic" for
    an example on how to wrap non-monotonic clocks to be monotonic.
-}
data Clock c = Clock {
    -- | Get the current time.
    clockNow   :: !(c Tick)
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
  , clockDelay :: !(TickDelta -> c ())
    {-| Interleave actions with ticks.

        This is typically recommended for the use-case where your action
        represents a stream of inputs, e.g. from the network or the user. It
        is meant to satisfy the same functionality as the @select@ system call
        found in common operating systems, used with a timeout parameter.

        If @action@ when executed repeatedly gives a sequence of results, then
        in the expression @cAct <- clock `clockWith` action@, @runClocked cAct@
        when executed repeatedly gives the same sequence of elements but with
        ticks interleaved in between them. Executing @finClocked cAct@ closes
        any resources and invalidates any future calls to @cAct@.

        It is not necessary to call @finClocked@ if any part of @runClocked@
        (e.g. child threads) throws an exception - implementations should
        detect these situations and clean these up automatically. This frees
        the user of this function from requiring extra constraints which would
        be necessary if it's necessary to run @`finally` finClocked cAct@ as
        cleanup.
    -}
  , clockWith  :: !(forall a. c a -> c (Clocked c a))
    {-| Given an action, run it with a timeout.

        This is typically recommended for the use-case where your action
        represents the response to a single previously-sent request.

        The action may complete despite the timeout firing, in which case its
        result will be lost. This is in general unavoidable and is a common
        property that one simply has to live with in distributed systems. If
        you run the input action repeatedly, then this property applies *for
        every execution*, i.e. it is possible that you get 10 timeouts even
        though the action succeeded 10 times, and you'll lose 10 results.

        If you want all results of all actions, use @clockWith@ instead. The
        downside with that, is that it's slightly less efficient than this, as
        it will interleave every single 'Tick' event and it is up to you to
        deal with skipping/ignoring any of them.
    -}
  , clockTimer :: !(forall a. TickDelta -> c a -> c (Either Tick a))
}

clockTick :: Monad c => Clock c -> TickDelta -> c Tick
clockTick clock d = clockDelay clock d >> clockNow clock

-- | See 'clockWith' for details on what this is for.
data Clocked c a = Clocked {
    runClocked :: !(c (Either Tick a))
  , finClocked :: !(c ())
  }
