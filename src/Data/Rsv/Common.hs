{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Rsv.Common
  ( RHandles(..)
  , RHandle
  , newHandles
  , nextHandle
  , checkHandle
  , withHandle
  , sEnqueue
  , sUnqueue
  , sDequeue
  )
where

-- external
import qualified Data.Sequence.Internal as Seq

import           Data.Sequence          (Seq (..), (|>))
import           Data.Word              (Word64)
import           GHC.Generics           (Generic)
import           GHC.Stack              (HasCallStack)


-- Handle generator. Runtime invariants:
--
-- 1. A handle from one generator is not used in a context that expects a
-- handle from a different generator. TODO: use a string or other data to
-- distinguish the contexts.
--
-- 2. Newly generated handles are distinguishable from previously-generated
-- ones. 'checkHandle' is used to check this.
newtype RHandles = RHandles { getNextHandle :: RHandle }
  deriving (Show, Read, Generic, Eq)
newtype RHandle = RHandle { getHandle :: Word64 }
  deriving (Show, Read, Generic, Eq, Ord, Enum, Bounded)

newHandles :: RHandles
newHandles = RHandles (RHandle 0)

nextHandle :: RHandles -> (RHandle, RHandles)
nextHandle (RHandles h) = (h, RHandles (succ h))

-- | Check that an existing handle is consistent with the current state of a
-- handle generator, i.e. it must not be part of the generator's future.
checkHandle :: RHandles -> RHandle -> Bool
checkHandle (RHandles hh) h = hh > h

withHandle
  :: ((RHandle, i) -> c -> c) -> i -> (RHandles, c) -> (RHandle, (RHandles, c))
withHandle doWith item (handles0, container0) =
  let (handle, handles1) = nextHandle handles0
      container1         = doWith (handle, item) container0
  in  (handle, (handles1, container1))

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
    _                       -> error "sUnqueue found more than one key"

sDequeue :: Seq a -> (Maybe a, Seq a)
sDequeue (h :<| t) = (Just h, t)
sDequeue Seq.Empty = (Nothing, Seq.Empty)
