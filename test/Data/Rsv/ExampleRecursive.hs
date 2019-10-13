{-| Example on how to do self-deleting values. -}

module Data.Rsv.ExampleRecursive where

import           Data.Rsv.RList                 ( Delete
                                                , RList
                                                , insert
                                                , toList
                                                )


-- | Alias for copy+paste convenience, in case anyone wants to adapt the below
-- to other data structures
type Container a = RList a

{-| The type you'd like to insert

    More specifically, this is a function that generates a self-deleting
    value of type @a@, which could be e.g. a monadic action that executes the
    first argument (@DeleteRec a@) on the current container.
-}
type Rec a = DeleteRec a -> a
-- | The actual type stored in the container
data RecRef a = RecC (Rec a) (DeleteRec a)

type ContainerRec a = Container (RecRef a)
type DeleteRec a = Delete (RecRef a)

-- | Insert a self-deleting value
insert_ :: Rec a -> ContainerRec a -> (DeleteRec a, ContainerRec a)
insert_ v m =
  let rec       = RecC v del -- takes advantage of laziness
      (del, m2) = insert rec m
  in  (del, m2)

-- | Insert a regular non-self-deleting value
insert' :: a -> ContainerRec a -> (DeleteRec a, ContainerRec a)
insert' = insert_ . const

deRef :: RecRef a -> a
deRef (RecC v del) = v del

-- | Get all values
toList' :: ContainerRec a -> [a]
toList' = fmap deRef . toList
