-- TODO: export to upstream primitive

{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

{-| Extra utilities and abstractions for "Control.Monad.Primitive".

The API structure is stable, but __the naming is not great and may change__.
Ideally we would push this upstream into "Control.Monad.Primitive" itself.
-}
module Control.Monad.Primitive.Extra
  ( PrimST(..)
  , readPrimST
  , writePrimST
  , modifyPrimST
  , stMutVar
  , module Control.Monad.Primitive
  )
where

import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Data.Primitive.MutVar
import           Data.Tuple              (swap)
import           Data.Tuple.Extra        (dupe)


-- | Type abstracting a mutable reference.
--
-- This can be thought of as a mutable version of a @Lens' (PrimState m) s@
-- with the lens functor specialised to @(,) a@ for each @a@.
newtype PrimST m s = PrimST { statePrimST :: forall a. (s -> (a, s)) -> m a }

readPrimST :: PrimST m s -> m s
readPrimST st = statePrimST st dupe

writePrimST :: PrimST m s -> s -> m ()
writePrimST st s1 = statePrimST st (const ((), s1))

modifyPrimST :: PrimST m s -> (s -> s) -> m ()
modifyPrimST st f = statePrimST st (((), ) . f)

stMutVar :: PrimMonad m => MutVar (PrimState m) s -> PrimST m s
stMutVar mv = PrimST (atomicModifyMutVar' mv . (swap .))
