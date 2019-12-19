{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-| Examples for various 'Data.Rsv' structures. -}

module Data.Rsv.Example where

-- external
import           Control.Monad.Trans.State.Strict (evalStateT, state)
import           Data.Functor.Identity            (runIdentity)
import           Data.Maybe                       (isJust)

-- internal
import qualified Data.Rsv.RMMap                   as RM


-- | Basic usage example for 'RMMap'
--
-- >>> rMMapExample0
-- True
rMMapExample0 :: Bool
rMMapExample0 = runIdentity $ (`evalStateT` RM.empty) $ do
  sDelete           <- state $ RM.enqueue (1000, const "my callback")
  notAlreadyRemoved <- state $ RM.unqueue sDelete
  return $ isJust notAlreadyRemoved
