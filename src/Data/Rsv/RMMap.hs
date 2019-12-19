{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

{-| See "Data.Rsv" for an overview of what "reservation" data structures are.

This module implements a multi-map, where each insert is indexed by a key, and
many independent inserts may be performed on the same key.

-}

module Data.Rsv.RMMap
  ( RMMap(..)
  , handles_
  , content_
  , Delete
  , empty
  -- * Read operations
  , isEmpty
  , (!)
  -- * Write operations
  , enqueue
  , unqueue
  , dequeue
  )
where

-- external
import           Control.Lens    (Iso, anon, at, iso, makeLensesFor, (%%~),
                                  (%~), (&))
import           Data.Bifunctor  (first)
import           GHC.Generics    (Generic)

import qualified Data.Map.Strict as M
import           Data.Sequence   (Seq (..))

-- internal
import           Data.Rsv.Common


type Entries a = Seq (RHandle, a)

data RMMap k a = RMMap {
  handles :: !RHandles,
  content :: !(M.Map k (Entries a))
} deriving (Show, Read, Generic, Eq)
makeLensesFor ((\x -> (x, x ++ "_")) <$> ["handles", "content"]) ''RMMap

toPair
  :: Iso
       (RMMap k0 a0)
       (RMMap k1 a1)
       (RHandles, M.Map k0 (Entries a0))
       (RHandles, M.Map k1 (Entries a1))
toPair = iso (\(RMMap x y) -> (x, y)) (uncurry RMMap)

empty :: RMMap k a
empty = RMMap { handles = newHandles, content = M.empty }

isEmpty :: RMMap k a -> Bool
isEmpty sm = M.null m || all null m where m = content sm

(!) :: Ord k => RMMap k a -> k -> Seq a
m ! k = case M.lookup k $ content m of
  Just l  -> snd <$> l
  Nothing -> mempty

data Delete k a = Delete !k !RHandle
  deriving (Show, Read, Generic, Eq, Ord)

-- | Append an item on a key, returning a handle to remove it with.
-- The same item may be added twice, in which case it will occupy multiple
-- positions in the map, and the handles distinguish these occurences.
enqueue :: Ord k => (k, a) -> RMMap k a -> (Delete k a, RMMap k a)
enqueue i@(k, _) m = m & toPair %%~ withHandle enq i & first (Delete k)
 where
  enq
    :: Ord k => (RHandle, (k, a)) -> M.Map k (Entries a) -> M.Map k (Entries a)
  enq (h', (k', v')) m' = m' & at k' . anon mempty null %~ sEnqueue (h', v')

-- | Delete an item corresponding to a given handle.
-- If the item was already removed, 'Nothing' is returned instead.
unqueue :: Ord k => Delete k a -> RMMap k a -> (Maybe a, RMMap k a)
unqueue (Delete k idx) m =
  m & content_ . at k . anon mempty null %%~ sUnqueue idx

-- | Remove an item from a key, from the front. Return Nothing if key is empty.
dequeue :: Ord k => k -> RMMap k a -> (Maybe (Delete k a, a), RMMap k a)
dequeue k m =
  m
    & (content_ . at k . anon mempty null %%~ sDequeue)
    & (first (fmap (first (Delete k))))
