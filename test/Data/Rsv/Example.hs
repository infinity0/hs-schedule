{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-| Examples for various 'Data.Rsv' structures. -}

module Data.Rsv.Example where

-- external
import           Control.Monad.Trans.State.Strict (evalStateT, state)
import           Data.Functor.Identity            (runIdentity)
import           Data.Maybe                       (isJust)

-- internal
import qualified Data.Rsv.RList                   as L
import qualified Data.Rsv.RMMap                   as M


-- | Basic usage example for 'RList'
--
-- >>> rListExample0
-- True
rListExample0 :: Bool
rListExample0 = runIdentity $ (`evalStateT` L.empty) $ do
  sDelete           <- state $ L.insert $ const "my callback"
  notAlreadyRemoved <- state $ L.delete sDelete
  return $ isJust notAlreadyRemoved

-- | Basic usage example for 'RMMap'
--
-- >>> rMMapExample0
-- True
rMMapExample0 :: Bool
rMMapExample0 = runIdentity $ (`evalStateT` M.empty) $ do
  sDelete           <- state $ M.enqueue (1000, const "my callback")
  notAlreadyRemoved <- state $ M.unqueue sDelete
  return $ isJust notAlreadyRemoved
