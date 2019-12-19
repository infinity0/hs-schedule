{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Rsv.Common
  ( RHandles(..)
  , RHandle
  , newHandles
  , nextHandle
  , withHandle
  , alInsert
  , alDelete
  , alDelete'
  , sEnqueue
  , sUnqueue
  , sDequeue
  )
where

-- external
import           Data.Maybe             (fromMaybe)
import           Data.Word              (Word64)
import           GHC.Generics           (Generic)
import           GHC.Stack              (HasCallStack)

import           Data.Sequence          (Seq (..), (|>))
import qualified Data.Sequence.Internal as Seq


-- TODO: this could be more secure like using a PRNG or something
newtype RHandles = RHandles { getNextHandle :: RHandle }
  deriving (Show, Read, Generic, Eq)
newtype RHandle = RHandle { getHandle :: Word64 }
  deriving (Show, Read, Generic, Eq, Ord, Enum)

newHandles :: RHandles
newHandles = RHandles (RHandle 0)

nextHandle :: RHandles -> (RHandle, RHandles)
nextHandle (RHandles h) = (h, RHandles (succ h))

withHandle
  :: ((RHandle, i) -> c -> c) -> i -> (RHandles, c) -> (RHandle, (RHandles, c))
withHandle doWith item (handles0, container0) =
  let (handle, handles1) = nextHandle handles0
      container1         = doWith (handle, item) container0
  in  (handle, (handles1, container1))

alInsert :: k -> a -> Maybe [(k, a)] -> Maybe [(k, a)]
alInsert k' v' slm = Just $ (k', v') : fromMaybe [] slm

alDelete :: Eq k => k -> Maybe [(k, a)] -> (Maybe a, Maybe [(k, a)])
alDelete idx' slm = case alDelete' idx' $ fromMaybe [] slm of
  (d, []) -> (d, Nothing)
  (d, x ) -> (d, Just x)

alDelete' :: Eq k => k -> [(k, a)] -> (Maybe a, [(k, a)])
alDelete' _ [] = (Nothing, [])
alDelete' idx (h : t) =
  if fst h == idx then (Just (snd h), t) else (h :) <$> alDelete' idx t

sEnqueue :: a -> Seq a -> Seq a
sEnqueue x slm = slm |> x

sUnqueue :: (HasCallStack, Eq k) => k -> Seq (k, a) -> (Maybe a, Seq (k, a))
sUnqueue idx' slm = (snd <$> found', others)
 where
  -- TODO: this is O(n); maybe it should be more efficient...
  (Seq.Seq found, others) = Seq.partition ((== idx') . fst) $ slm
  found'                  = case found of
    Seq.EmptyT              -> Nothing
    Seq.Single (Seq.Elem x) -> Just x
    _                       -> error "delete found more than one key"

sDequeue :: Seq a -> (Maybe a, Seq a)
sDequeue (h :<| t) = (Just h, t)
sDequeue Seq.Empty = (Nothing, Seq.Empty)
