{-# LANGUAGE TemplateHaskell #-}

{-| See "Data.Rsv" for an overview of what "reservation" data structures are.

This module implements a multi-map, where each insert is indexed by a key, and
many independent inserts may be performed on the same key.

-}

module Data.Rsv.RMMap (
    RMMap
  , empty
  -- * Read operations
  , isEmpty
  , (!)
  -- * Write operations
  , insert
  , Delete
  , deleteKey
  -- * Write operations in 'StateT'
  , sInsert
  , SDelete
  , sDeleteKey
  ) where

-- external
import Control.Arrow ((***), first)
import Control.Lens (makeLensesFor, (&), (%~), (%%~), at)
import Control.Monad.Trans.State.Strict (StateT, state, modify)
import Data.Maybe (fromMaybe)

-- ours
import qualified Data.Map.Strict as M
import Data.Rsv.RList (RList)
import qualified Data.Rsv.RList as RsvL


data RMMap k a = RMMap {
    subMap :: M.Map k (RList a)
}
makeLensesFor ((\x -> (x, x ++ "_")) <$> ["subMap"]) ''RMMap

-- | An empty 'RMMap'.
empty :: RMMap k a
empty = RMMap { subMap = M.empty }

isEmpty :: RMMap k a -> Bool
isEmpty sm = M.null m || all RsvL.isEmpty m where m = subMap sm

(!) :: Ord k => RMMap k a -> k -> [a]
m ! k = case M.lookup k $ subMap m of
    Just l -> RsvL.toList l
    Nothing -> []

-- | A map-transition that removes and retrieves the earlier-added item.
-- If the item was already removed, 'Nothing' is returned instead.
type Delete k a = RMMap k a -> (Maybe a, RMMap k a)

-- | Add an item to the list, returning a handle to remove it with.
-- The same item may be added twice, in which case it will occupy multiple
-- positions in the map, and the handles distinguish these occurences.
insert :: Ord k => k -> a -> RMMap k a -> (Delete k a, RMMap k a)
insert k v m =
    m & subMap_ . at k %%~ listInsert k v where
    -- The above is a bit of lens magic, 'm & at k %%~ f v' is basically what
    -- '(fst . f, Map.alter (snd . f) k m)' intuitively looks like it's supposed
    -- to do, but of course can't work in that form because we need to make the
    -- result of f escape Map.alter. The lens-based solution should be quicker
    -- than traversing the map twice via the ordinary Map functions.
    listInsert :: Ord k => k -> a -> Maybe (RList a) -> (Delete k a, Maybe (RList a))
    listInsert k' v' slm = mapDelete k' *** Just $ RsvL.insert v' baseSl where
        baseSl = fromMaybe RsvL.empty slm

mapDelete :: Ord k => k -> RsvL.Delete a -> Delete k a
mapDelete k listDelete m =
    m & subMap_ . at k %%~ tweak listDelete where
    tweak :: RsvL.Delete a -> Maybe (RList a) -> (Maybe a, Maybe (RList a))
    tweak listDelete' slm = maybeDelete <$> listDelete' baseSl where
        baseSl = fromMaybe RsvL.empty slm

maybeDelete :: RList a -> Maybe (RList a)
maybeDelete = Just
--maybeDelete sl = if SL.isEmpty sl then Nothing else Just sl
-- TODO: we cannot do this yet, since we might (for a given key k):
--   1. remove all its items (A)
--   2. add another item b
--   3. call the delete-handle on a previous item from A that was given the
--      same internal sublist index as b
-- To avoid this we currently keep the sublists around, which is not ideal. The
-- ideal fix is to either:
--   a. make the sublist indexes not Ints but some truly unique object, or
--   b. track the unique indexes in *this* structure
-- Both are too complex to bother with at this time; we'll revisit this if
-- people run into memory issues with our lazy cleanup.

deleteKey :: Ord k => k -> RMMap k a -> RMMap k a
deleteKey k m = m & subMap_ %~ M.delete k

-- | Same as 'Delete' except in the 'StateT' monad
type SDelete k a m = StateT (RMMap k a) m (Maybe a)

-- | Same as 'insert' except in the 'StateT' monad
sInsert :: (Ord k, Monad m) => k -> a -> StateT (RMMap k a) m (SDelete k a m)
sInsert k e = state $ first state . insert k e

-- | Same as 'deleteKey' except in the 'StateT' monad
sDeleteKey :: (Ord k, Monad m) => k -> StateT (RMMap k a) m ()
sDeleteKey = modify . deleteKey
