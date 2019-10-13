{-# LANGUAGE DeriveGeneric              #-}
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
import           Data.Maybe                     ( fromMaybe )
import           Data.Word                      ( Word64 )
import           GHC.Generics                   ( Generic )

import           Data.Sequence                  ( Seq(..)
                                                , (|>)
                                                )
import qualified Data.Sequence.Internal        as Seq


-- TODO: this could be more secure like using a PRNG or something
newtype RHandles = RHandles { getNextHandle :: RHandle }
  deriving (Eq, Show, Generic)
newtype RHandle = RHandle { getHandle :: Word64 }
  deriving (Eq, Show, Generic, Enum)

newHandles :: RHandles
newHandles = RHandles (RHandle 0)

nextHandle :: RHandles -> (RHandle, RHandles)
nextHandle (RHandles h) = (h, RHandles (succ h))

withHandle
  :: ((RHandle, i) -> c -> c) -> i -> (RHandles, c) -> (RHandle, (RHandles, c))
withHandle doWith item (handles0, container0) =
  let (handle, handles1) = nextHandle handles0
  in  let container1 = doWith (handle, item) container0
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

sEnqueue :: a -> Maybe (Seq a) -> Maybe (Seq a)
sEnqueue x slm = Just $ fromMaybe mempty slm |> x

sUnqueue :: Eq k => k -> Maybe (Seq (k, a)) -> (Maybe a, Maybe (Seq (k, a)))
sUnqueue idx' slm = (snd <$> found', others')
 where
  (Seq.Seq found, others) =
    Seq.partition ((== idx') . fst) $ fromMaybe mempty slm
  found' = case found of
    Seq.EmptyT              -> Nothing
    Seq.Single (Seq.Elem x) -> Just x
    _                       -> error "delete found more than one key"
  others' = case others of
    Seq.Empty -> Nothing
    x         -> Just x

sDequeue :: Maybe (Seq a) -> (Maybe a, Maybe (Seq a))
sDequeue (Just (h :<| t)) = (Just h, Just t)
sDequeue _                = (Nothing, Nothing)
