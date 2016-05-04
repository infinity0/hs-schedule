{-# LANGUAGE TemplateHaskell #-}

{-| See "Data.Rsv" for an overview of what "reservation" data structures are.

This module implements a list, where each insert is appended to its tail.

-}

module Data.Rsv.RList (
    RList
  , empty
  -- * Read operations
  , isEmpty
  , toList
  -- * Write operations
  , insert
  , Delete
  -- * Write operations in 'StateT'
  , sInsert
  , SDelete
  ) where

-- external
import Control.Arrow (first)
import Control.Lens (makeLensesFor, (&), (%~), (+~), (%%~))
import Control.Monad.Trans.State.Strict (StateT, state)


data RList a = RList {
    nextIndex :: Int,
    subList :: [(Int, a)] -- earliest inserted is at the tail
    -- external views must reverse this ordering
}
makeLensesFor ((\x -> (x, x ++ "_")) <$> ["nextIndex", "subList"]) ''RList

-- | An empty 'RList'.
empty :: RList a
empty = RList { nextIndex = 0, subList = [] }

isEmpty :: RList a -> Bool
isEmpty sl = null (subList sl)

toList :: RList a -> [a]
-- reverse the order, so the earliest inserted is at the head
toList l = snd <$> reverse (subList l)

-- | A list-transition that removes and retrieves the earlier-added item.
-- If the item was already removed, 'Nothing' is returned instead.
type Delete a = RList a -> (Maybe a, RList a)

-- | Add an item to the list, returning a handle to delete it with.
-- The same item may be added twice, in which case it will occupy multiple
-- positions in the list, and the handles distinguish these occurences.
insert :: a -> RList a -> (Delete a, RList a)
insert v sl = let idx = nextIndex sl in
    (deleteAt idx, sl & nextIndex_ +~ 1 & subList_ %~ ((idx, v) :))

deleteAt :: Int -> RList a -> (Maybe a, RList a)
deleteAt idx l = l & subList_ %%~ deleteAt_ idx

deleteAt_ :: Int -> [(Int, a)] -> (Maybe a, [(Int, a)])
deleteAt_ _ [] = (Nothing, [])
deleteAt_ idx (h:t) = if fst h == idx
    then (Just (snd h), t)
    else (h :) <$> deleteAt_ idx t

-- | Same as 'Delete' except in the 'StateT' monad
type SDelete a m = StateT (RList a) m (Maybe a)

-- | Same as 'insert' except in the 'StateT' monad
sInsert :: Monad m => a -> StateT (RList a) m (SDelete a m)
sInsert v = state $ first state . insert v
