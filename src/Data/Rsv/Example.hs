{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-| Examples for various 'Data.Rsv' structures. -}

module Data.Rsv.Example where

import qualified Data.Rsv.RList as L
import qualified Data.Rsv.RMMap as M
import qualified Data.Rsv.RRelMMap as RelM

import Control.Monad.Trans.State.Strict (evalStateT)
import Data.Functor.Identity (runIdentity)
import Data.Maybe (isJust)
import Data.Monoid (Sum(..))


-- | Basic usage example for 'RList'
--
-- >>> rListExample0
-- True
rListExample0 :: Bool
rListExample0 = runIdentity $ (`evalStateT` L.empty) $ do
    sDelete <- L.sInsert $ const "my callback"
    notAlreadyRemoved <- sDelete
    return $ isJust notAlreadyRemoved

-- | Basic usage example for 'RMMap'
--
-- >>> rMMapExample0
-- True
rMMapExample0 :: Bool
rMMapExample0 = runIdentity $ (`evalStateT` M.empty) $ do
    sDelete <- M.sInsert 1000 $ const "my callback"
    notAlreadyRemoved <- sDelete
    return $ isJust notAlreadyRemoved

-- | Basic usage example for 'RRelMMap'
--
-- >>> rRelMMapExample0
-- True
rRelMMapExample0 :: Bool
rRelMMapExample0 = runIdentity $ (`evalStateT` RelM.empty) $ do
    sDelete <- RelM.sInsertAfter (Sum 1000) $ const "my callback"
    notAlreadyRemoved <- sDelete
    return $ isJust notAlreadyRemoved
