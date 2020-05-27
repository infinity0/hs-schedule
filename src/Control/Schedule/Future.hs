{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

{-| Pure serialisable futures.

__This API is experimental at the moment, and parts of it may change.__
-}
module Control.Schedule.Future where

import qualified Data.Map.Strict                  as M
import qualified Data.Set                         as S

-- external
import           Codec.Serialise                  (Serialise)
import           Control.Lens                     (IndexedTraversal', Lens', at,
                                                   contains, indices, (%%~),
                                                   (%~), (.~), (?~), (^.))
import           Control.Lens.TH                  (makeLensesFor, makePrisms)
import           Control.Monad.Trans.State.Strict (runState, state)
import           Data.Binary                      (Binary)
import           Data.Function                    ((&))
import           Data.Schedule                    (Schedule, Task, TickDelta,
                                                   after, cancel_)
import           GHC.Generics                     (Generic)
import           GHC.Stack                        (HasCallStack)
import           Safe                             (fromJustNote)


type OSet a = S.Set a -- FIXME: should be set ordered by insertion time
type OMap k v = M.Map k v -- FIXME: should be map ordered by insertion time

{- | A future value that might time out.

None of the other functions in this module use this, but it is provided for
convenience, potentially as the @ro@ parameter of 'SFuture'.
-}
data TimedResult to r =
    TimedOut !to
  | GotResult !r
  deriving (Show, Read, Generic, Binary, Serialise, Eq, Ord)

{- | A future result.

An 'SFuture' tracks either the 'SExpect's that are waiting on its result,
indexed by type @wo@; or else the result that has been given back.
-}
data SFuture wo ro =
    SFWaiting !(OSet wo)
    -- ^ SExpects waiting on us
  | SFSettled !ro
    -- ^ Result of the Future
  deriving (Show, Read, Generic, Binary, Serialise, Eq, Ord)
makePrisms ''SFuture

{- | An expectation on future results.

An 'SExpect' tracks the 'SFuture's that it's waiting on, indexed by type @wi@,
along with the results of the 'SFuture's that it previously waited on, that
have now given a result back.
-}
data SExpect wi tk = SExpect {
    seExpects :: !(OMap wi (Task tk))
    -- ^ SFutures we're waiting for, with our timeout waiting for them.
    --
    -- Note that the 'SFuture' might have its own separate timeout which is
    -- different; this @t@ timeout is when *we* stop waiting on it.
  } deriving (Show, Read, Generic, Binary, Serialise, Eq, Ord)
makeLensesFor ((\x -> (x, "_" <> x)) <$> ["seExpects"]) ''SExpect

instance Ord wi => Semigroup (SExpect wi tk) where
  s1 <> s2 = SExpect (seExpects s1 <> seExpects s2)

instance Ord wi => Monoid (SExpect wi tk) where
  mempty = SExpect mempty

data SFStatus e = Expecting e | NotExpecting deriving (Show, Read, Generic, Binary, Serialise, Eq, Ord)
type SFStatusFull wo tk = SFStatus (OSet wo, Task tk)

data SFError =
    SFEAlreadySettled
  | SFEInvalidPrecondition {
        sfePreExpect :: !(SFStatus ())
      , sfePreActual :: !(SFStatus ())
    }
  deriving (Show, Read, Generic, Binary, Serialise, Eq, Ord)

sCheckStatus
  :: (HasCallStack, Ord wi, Ord wo)
  => Lens' s (SFuture wo r)
  -> Lens' s (SExpect wi tk)
  -> wi
  -> wo
  -> s
  -> SFStatusFull wo tk
sCheckStatus lsf lse sfi sei s =
  case (s ^. lsf, s ^. lse . _seExpects . at sfi) of
    (SFSettled _      , Just _ ) -> error "SFuture result but SExpect expects"
    (SFWaiting waiting, Just lt) -> if waiting ^. contains sei
      then Expecting (waiting, lt)
      else error "SFuture not waiting but SExpect expects"
    (SFSettled _      , Nothing) -> NotExpecting
    (SFWaiting waiting, Nothing) -> if waiting ^. contains sei
      then error "SFuture waiting but SExpect not expects"
      else NotExpecting

{- | Make an 'SExpect' start waiting on an 'SFuture'.

Both the 'SExpect' and the 'SFuture' must already exist, and be accessible
via the given lens.

Returns:

  * @'Left' 'SFEInvalidPrecondition'@ if there was already an expectation.
  * @'Right' ('Just' r)@ if the 'SFuture' was already settled.
  * @'Right' 'Nothing'@ otherwise.
-}
sExpectFuture
  :: (Ord wi, Ord wo)
  => Lens' s (SFuture wo r)
  -> Lens' s (SExpect wi tk)
  -> Lens' s (Schedule tk)
  -> wi
  -> wo
  -> TickDelta
  -> tk
  -> s
  -> (Either SFError (Maybe r), s)
sExpectFuture lsf lse lsch sfi sei d tk s0 = case status of
  Expecting _ ->
    (Left $ SFEInvalidPrecondition NotExpecting (Expecting ()), s0)
  NotExpecting -> case s0 ^. lsf of
    SFWaiting sfWaiting ->
      let (lt, s1) = s0 & lsch %%~ after d tk
          s2 =
              s1
                -- SExpect add expecting, set timeout
                & (lse . _seExpects . at sfi ?~ lt)
                -- SFuture add sfWaiting
                & (lsf .~ SFWaiting (sfWaiting & contains sei .~ True))
      in  (Right Nothing, s2)
    SFSettled r -> do
      (Right (Just r), s0)
  where status = sCheckStatus lsf lse sfi sei s0

{- | Make an 'SExpect' stop waiting on an 'SFuture', e.g. after a timeout.

Both the 'SExpect' and the 'SFuture' must already exist, and be accessible
via the given lens.

This does not cancel the future itself, since other 'SExpect's may be waiting
on it. The caller can check if this is the case, and if not it may choose to
either cancel the future, or leave it running for a while (e.g. until it times
out), in case other 'SExpect's wait on it in the near future.

Note that this is distinct from any timeout on the 'SFuture' itself.
-}
sExpectCancel
  :: (Ord wi, Ord wo)
  => Lens' s (SFuture wo r)
  -> Lens' s (SExpect wi tk)
  -> Lens' s (Schedule tk)
  -> wi
  -> wo
  -> s
  -> (Either SFError (), s)
sExpectCancel lsf lse lsch sfi sei s0 = case status of
  NotExpecting ->
    (Left $ SFEInvalidPrecondition (Expecting ()) NotExpecting, s0)
  Expecting (sfWaiting, lt) ->
    let s1 =
            s0
            -- SExpect drop expects, clear timeout
              & (lsch %~ (snd . cancel_ lt))
              & (lse . _seExpects . at sfi .~ Nothing)
            -- SFuture drop sfWaiting
              & (lsf .~ SFWaiting (sfWaiting & contains sei .~ False))
    in  (Right (), s1)
  where status = sCheckStatus lsf lse sfi sei s0

{- | Set the result of a future, for all 'SExpect' that are waiting on it.

This also changes the 'SFuture' into a 'SFSettled', but does not clean it up as
the caller may want this to be done later. If another 'SExpect' wants to wait
on it before cleanup via 'sExpectFuture', it would get the result immediately.

Returns:

  * @'Left' 'SFEAlreadySettled'@ if the future was already settled.
  * @'Right' waiting@ of the previously-waiting 'SExpect's.
-}
sFutureSettled
  :: (Ord wi, Ord wo)
  => Lens' s (SFuture wo r)
  -> IndexedTraversal' wo s (SExpect wi tk)
  -> Lens' s (Schedule tk)
  -> wi
  -> r
  -> s
  -> (Either SFError (OSet wo), s)
sFutureSettled lsf lsse lsch sfi r s0 = case s0 ^. lsf of
  SFSettled _ -> (Left SFEAlreadySettled, s0)
  SFWaiting w ->
    let sch0       = s0 ^. lsch
        (s2, sch1) = f sch0
        s3         = s2 & lsch .~ sch1
        -- TODO: iterate in order of w, not the traversal
        f          = runState $ s0 & lsse . indices (`S.member` w) %%~ \se -> do
          -- SExpect drop expects, clear timeout
          let (lt', se') = se & _seExpects . at sfi %%~ (, Nothing)
              lt = fromJustNote "SFuture idx not found in SExpect expects" lt'
          state $ cancel_ lt
          pure $ se'
    in  (Right w, s3 & lsf .~ SFSettled r)
