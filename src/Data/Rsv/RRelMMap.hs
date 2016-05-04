{-# LANGUAGE TemplateHaskell #-}

{-| See "Data.Rsv" for an overview of what "reservation" data structures are.

This module implements a relative-key multi-map, where each structure has an
idea of "the current key", each insert is indexed by a key-offset relative to
the current, and many independent inserts may be performed on the same key.

Currently keys are constrainted by @('RMMap' k, 'Monoid' k)@. If you have trouble
finding a 'Monoid' instance for a 'Num' type, that's because it's not there;
see <https://wiki.haskell.org/Monoid#Examples> on why. In practise, you can
probably just wrap your 'Num' in a 'Data.Monoid.Sum'.

-}

module Data.Rsv.RRelMMap (
    RRelMMap
  , empty
  -- * Read operations
  , isEmpty
  , current
  , (!)
  -- * Write operations
  , setCurrent
  , insertAfter
  , Delete
  , deleteKey
  -- * Write operations in 'StateT'
  , sSetCurrent
  , sInsertAfter
  , SDelete
  , sDeleteKey
  ) where

-- external
import Control.Arrow (first)
import Control.Lens (makeLensesFor, (&), (%~), (.~), (%%~))
import Control.Monad.Trans.State.Strict (StateT, state, modify)

-- ours
import Data.Rsv.RMMap (RMMap)
import qualified Data.Rsv.RMMap as RsvM


data RRelMMap k a = RRelMMap {
    current :: k,
    subMap :: RMMap k a
}
makeLensesFor ((\x -> (x, x ++ "_")) <$> ["current", "subMap"]) ''RRelMMap

-- | An empty 'RRelMMap'.
empty :: (Ord k, Monoid k) => RRelMMap k a
empty = RRelMMap { current = mempty, subMap = RsvM.empty }

isEmpty :: RRelMMap k a -> Bool
isEmpty ts = RsvM.isEmpty $ subMap ts

(!) :: Ord k => RRelMMap k a -> k -> [a]
m ! k = subMap m RsvM.! k

-- | Advance to a given key
setCurrent :: (Ord k) => k -> RRelMMap k a -> RRelMMap k a
setCurrent k m = m & current_ .~ k

-- | A map-transition that removes and retrieves the earlier-added item.
-- If the item was already removed, 'Nothing' is returned instead.
type Delete k a = RRelMMap k a -> (Maybe a, RRelMMap k a)

insertAfter :: (Ord k, Monoid k) => k -> a -> RRelMMap k a -> (Delete k a, RRelMMap k a)
insertAfter k v m = let k2 = current m `mappend` k in
    first mapDelete $ m & subMap_ %%~ RsvM.insert k2 v

mapDelete :: Ord k => RsvM.Delete k a -> Delete k a
mapDelete delete m = m & subMap_ %%~ delete

deleteKey :: Ord k => k -> RRelMMap k a -> RRelMMap k a
deleteKey k m = m & subMap_ %~ RsvM.deleteKey k

-- | Same as 'setCurrent' except in the 'StateT' monad
sSetCurrent :: (Ord k, Monad m) => k -> StateT (RRelMMap k a) m ()
sSetCurrent = modify . setCurrent

-- | Same as 'Delete' except in the 'StateT' monad
type SDelete k a m = StateT (RRelMMap k a) m (Maybe a)

-- | Same as 'insertAfter' except in the 'StateT' monad
sInsertAfter :: (Ord k, Monoid k, Monad m) => k -> a -> StateT (RRelMMap k a) m (SDelete k a m)
sInsertAfter k e = state $ first state . insertAfter k e

-- | Same as 'deleteKey' except in the 'StateT' monad
sDeleteKey :: (Ord k, Monad m) => k -> StateT (RRelMMap k a) m ()
sDeleteKey = modify . deleteKey
