{-| Monad transformer for co-operative scheduled computations, with a clean
pure-impure separation and minimal impure code.

Co-operative means that we do not interrupt computations that don't expect to
be interrupted, such as 3rd-party libraries that have no knowledge of this
monad. In other words, we don't throw asynchronous exceptions, and there is
no "background executor" that automatically runs these tasks, interleaving
them with your main computation in an order that would be hard to predict.

What this means in practise is that, in addition to /scheduling/ tasks to be
run in the future via 'after' and friends, you must also explicitly specify
when these tasks might be /executed relative/ to your main computation. For
that, you have these options:

- bind 'runTasks' in your main computation, or
- use a 'LiftRT' to transform a particular (blocking) action to also run tasks
  in parallel with that action. Currently we only support doing this for:

    - @IO@ actions running under @IO@ clocks, using 'ioLiftRT'

    but hopefully we'll support others in the future, such as for "Pipes" that
    are blocked waiting for input. (This will likely be non-trivial, and my
    knowledge of complex Haskell control-flow structures is quite basic, so
    any help would be appreciated.)

For some concrete examples, see
<src/Control-Monad-Trans-Schedule-Example.html Control.Monad.Trans.Schedule.Example>
as well as our unit tests.

A consequence of our model, is that we make no guarantees about /precisely/
when tasks are run in terms of absolute time, and our API doesn't enable you
to express this. Instead, what you express is the /relative order/ in which
tasks should be run, which /is/ guaranteed; as well as an approximate mapping
from this onto real absolute time, which is honoured on a best-effort basis.
We believe this is much easier to reason about (and therefore safer) than the
opposite trade-off, in the vast majority of use-cases.

= Motivation

For many secure communications protocols, we want a guarantee of freshness. But
impure timeout-based code can be hard to analyse, and can have very complex
interactions with the rest of the protocol system. That's where this library
comes in, and that's why we made the design choices stated above, such as
avoiding of interrupts and requiring explicit user-controlled task execution.

Although motivated by these security goals, we intend this library to be a
generally-useful tool, that has the power to replace impure timeout mechanisms
like "System.Timeout" where appropriate.

= TODO

- define a proper 'MFunctor' instance for 'ScheduleT'
- implement a fake clock for testing, that one can inject tick values into
- define and prove laws for liftClock+liftRT, investigate typeclass instances

-}

module Control.Monad.Trans.Schedule (
    Tick
  , Clock(..)
  -- * Pure scheduled computation
  , TaskState
  , ScheduleT
  , after
  , renew
  , TaskCancel
  -- * Generic impure execution
  , LiftClock
  , getClockNow'
  , runTasks'
  , runScheduleT'
  , LiftRT
  -- * Impure execution with 'Control.Monad.Base.MonadBase'
  , baseLiftClock
  , getClockNow
  , runTasks
  , runScheduleT
  -- * Impure execution with 'Control.Monad.IO.Class.MonadIO'
  , ioLiftClock
  , ioLiftRT'
  , ioLiftRT
  ) where

import Control.Monad.Trans.Schedule.Internal
import Control.Monad.Trans.Schedule.Base
import Control.Monad.Trans.Schedule.IO
