-- TODO: export to upstream primitive

{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

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


newtype PrimST m s = PrimST { statePrimST :: forall a. (s -> (a, s)) -> m a }

readPrimST :: PrimST m s -> m s
readPrimST st = statePrimST st dupe

writePrimST :: PrimST m s -> s -> m ()
writePrimST st s1 = statePrimST st (const ((), s1))

modifyPrimST :: PrimST m s -> (s -> s) -> m ()
modifyPrimST st f = statePrimST st (((), ) . f)

stMutVar :: PrimMonad m => MutVar (PrimState m) s -> PrimST m s
stMutVar mv = PrimST (atomicModifyMutVar' mv . (swap .))
