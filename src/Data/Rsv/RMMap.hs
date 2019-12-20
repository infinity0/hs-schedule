{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

{-| This module implements a /reservation/ multi-map.

Each insert is indexed by a key; many inserts (of the same or different items)
may be performed on the same key.

A /reservation/ data structure is one that allows multiple inserts of the same
item, by returning a unique handle for each insert operation that must be given
to the deletion operation. If you need to store the handle together with the
item, e.g. so that the item knows how to delete itself, you can achieve this by
the standard Haskell "tying the knot" technique.
-}
module Data.Rsv.RMMap
  ( RMMap(..)
  , _handles
  , _content
  , Delete
  , checkValidity
  , checkHandle
  , empty
  -- * Read operations
  , isEmpty
  , (!)
  , toList
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
import qualified Data.Foldable   as F (toList)
import           Data.Maybe      (mapMaybe)
import           Data.Text       (Text, pack)
import           GHC.Generics    (Generic)

import qualified Data.Map.Strict as M
import           Data.Sequence   (Seq (..))

-- internal
import           Data.Rsv.Common hiding (checkHandle)
import qualified Data.Rsv.Common as R (checkHandle)


type Entries a = Seq (RHandle, a)

data RMMap k a = RMMap {
  handles :: !RHandles,
  content :: !(M.Map k (Entries a))
} deriving (Show, Read, Generic, Eq)
makeLensesFor ((\x -> (x, "_" <> x)) <$> ["handles", "content"]) ''RMMap

data Delete k a = Delete !k !RHandle
  deriving (Show, Read, Generic, Eq, Ord)

toPair
  :: Iso
       (RMMap k0 a0)
       (RMMap k1 a1)
       (RHandles, M.Map k0 (Entries a0))
       (RHandles, M.Map k1 (Entries a1))
toPair = iso (\(RMMap x y) -> (x, y)) (uncurry RMMap)

-- | Check the map that its internal invariants all hold.
--
-- You must run this on every instance obtained not via the API functions here.
-- For example, you must run this on instances obtained via deserialisation,
-- which in general cannot check the complex invariants maintained by the API
-- functions. Also, for all handles you obtain via a similarly non-standard
-- method, including by deserialisation of a parent data structure, you must
-- run @checkHandle map handle@.
--
-- @Nothing@ means the check passed, else @Just errmsg@ gives a failure reason.
--
-- Note: this does not guard against all malicious behaviour, but it does guard
-- against violation (either malicious or accidental) of the runtime invariants
-- assumed by this data structure.
checkValidity :: RMMap k a -> Maybe Text
checkValidity (RMMap handles' content') =
  let res = flip mapMaybe (M.toList content') $ \(k, hh) -> do
        if any (not . R.checkHandle handles' . fst) hh then Just k else Nothing
  in  case res of
        [] -> Nothing
        e  -> Just $ pack "some handles were reused in the input"

-- | Check that an existing handle is consistent with the current state of the
-- structure, i.e. it is not a handle that could be generated in the future.
checkHandle :: RMMap k a -> Delete k a -> Bool
checkHandle (RMMap handles' _) (Delete _ h) = R.checkHandle handles' h

empty :: RMMap k a
empty = RMMap { handles = newHandles, content = M.empty }

isEmpty :: RMMap k a -> Bool
isEmpty sm = M.null m || all null m where m = content sm

(!) :: Ord k => RMMap k a -> k -> Seq a
m ! k = case M.lookup k $ content m of
  Just l  -> snd <$> l
  Nothing -> mempty

toList :: RMMap k a -> [Delete k a]
toList (RMMap _ content') =
  M.toList content' >>= \(k, hh) -> F.toList hh & fmap (Delete k . fst)

-- | Append an item on a key, returning a handle to remove it with.
-- The same item may be added twice, in which case it will occupy multiple
-- positions in the map, and the handles distinguish these occurences.
enqueue :: Ord k => (k, a) -> RMMap k a -> (Delete k a, RMMap k a)
enqueue i@(k, _) m = m & toPair %%~ withHandle enq i & first (Delete k)
 where
  enq
    :: Ord k => (RHandle, (k, a)) -> M.Map k (Entries a) -> M.Map k (Entries a)
  enq (h', (k', v')) m' = m' & at k' . anon mempty null %~ sEnqueue (h', v')

req :: (a -> b) -> (Maybe a, c) -> (Maybe b, c)
req = first . fmap

-- | Delete an item corresponding to a given handle.
-- If the item was already removed, 'Nothing' is returned instead.
unqueue :: Ord k => Delete k a -> RMMap k a -> (Maybe (k, a), RMMap k a)
unqueue (Delete k idx) m =
  m & _content . at k . anon mempty null %%~ sUnqueue idx & req (k, )

-- | Remove an item from a key, from the front. Return Nothing if key is empty.
dequeue :: Ord k => k -> RMMap k a -> (Maybe (Delete k a, a), RMMap k a)
dequeue k m =
  m & _content . at k . anon mempty null %%~ sDequeue & req (first (Delete k))
