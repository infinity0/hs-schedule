{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE
    MultiParamTypeClasses
  , FlexibleInstances
  , UndecidableInstances
  #-}

{-| This module just defines an instance of 'MonadBase' for 'ComposeT'.

See also <https://github.com/mvv/transformers-base/issues/12>
-}
module Control.Monad.Base.Compose where

import Control.Monad.Base (MonadBase(liftBase), liftBaseDefault)
import Control.Monad.Morph (MFunctor)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Compose (ComposeT)

instance (
    MonadBase b m,
    MFunctor f,
    MonadTrans f,
    MonadTrans g,
    Monad (f (g m))
  ) => MonadBase b (ComposeT f g m) where
    liftBase = liftBaseDefault
