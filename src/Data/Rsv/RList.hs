{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

{-| See "Data.Rsv" for an overview of what "reservation" data structures are.

This module implements a list, where each insert is appended to its tail.

-}

module Data.Rsv.RList
  ( RList(..)
  , handles_
  , content_
  , Delete
  , empty
  -- * Read operations
  , isEmpty
  , toList
  -- * Write operations
  , insert
  , delete
  )
where

-- external
import           Control.Lens    (Iso, iso, makeLensesFor, (%%~), (&))
import           Data.Bifunctor  (first)
import           GHC.Generics    (Generic)

-- internal
import           Data.Rsv.Common


data RList a = RList {
  handles :: !RHandles,
  content :: ![(RHandle, a)]
    -- earliest inserted is at the tail
    -- external views must reverse this ordering
} deriving (Eq, Show, Generic)
makeLensesFor ((\x -> (x, x ++ "_")) <$> ["handles", "content"]) ''RList

toPair
  :: Iso
       (RList a0)
       (RList a1)
       (RHandles, [(RHandle, a0)])
       (RHandles, [(RHandle, a1)])
toPair = iso (\(RList x y) -> (x, y)) (uncurry RList)

empty :: RList a
empty = RList { handles = newHandles, content = mempty }

isEmpty :: RList a -> Bool
isEmpty sl = null (content sl)

toList :: RList a -> [a]
-- reverse the order, so the earliest inserted is at the head
toList l = snd <$> reverse (content l)

newtype Delete a = Delete RHandle
  deriving (Eq, Show, Generic)

-- | Add an item to the list, returning a handle to delete it with.
-- The same item may be added twice, in which case it will occupy multiple
-- positions in the list, and the handles distinguish these occurences.
insert :: a -> RList a -> (Delete a, RList a)
insert v m = m & toPair %%~ withHandle (:) v & first Delete

-- | Delete an item from the list corresponding to a given handle.
-- If the item was already removed, 'Nothing' is returned instead.
delete :: Delete a -> RList a -> (Maybe a, RList a)
delete (Delete idx) l = l & content_ %%~ alDelete' idx
