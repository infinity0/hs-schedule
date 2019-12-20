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
import           Control.Lens                     (IndexedTraversal', Lens', at,
                                                   contains, indices, (%%~),
                                                   (%~), (.~), (?~), (^.))
import           Control.Lens.TH                  (makeLensesFor, makePrisms)
import           Control.Monad.Trans.State.Strict (runState, state)
import           Data.Function                    ((&))
import           Data.Functor.Compose             (Compose (..))
import           Data.Schedule                    (Schedule, Task, TickDelta,
                                                   after, cancel_)
import           GHC.Generics                     (Generic)
import           GHC.Stack                        (HasCallStack)
import           Safe                             (fromJustNote)


type OSet a = S.Set a -- TODO: ideally should be set ordered by insertion time
type OMap k v = M.Map k v -- TODO: ideally should be map ordered by insertion time

data TimedResult tk r =
    TimedOut !tk
  | GotResult !r
   deriving (Show, Read, Generic, Eq, Ord)

data SFuture wo ro =
    SFWaiting !(OSet wo)
    -- ^ SExpects waiting on us
  | SFResult !ro
    -- ^ Result of the Future
  deriving (Show, Read, Generic, Eq, Ord)
makePrisms ''SFuture

data SExpect wi ri tk = SExpect {
    seExpects :: !(OMap wi (Task tk))
    -- ^ SFutures we're waiting for, with our own timeout.
    --
    -- Note that the SFuture might have its own separate timeout which is
    -- different; this @t@ timeout is when *we* stop waiting on it.
    --
    -- For example if @(i ~ TimedResult a)@ and our timeout is longer than
    -- their timeout then 'seResults' will get a @GotResult (TimedOut t)@.
  , seResults :: !(OMap wi (TimedResult tk ri))
    -- ^ SFutures that have completed, with the result. This is meant to be a
    -- holding place and the caller of this should move items from here into
    -- some other place to indicate that the results have been processed, so
    -- that if it is called twice it does not process these results twice.
  } deriving (Show, Read, Generic, Eq, Ord)
makeLensesFor ((\x -> (x, "_" <> x)) <$> ["seExpects", "seResults"]) ''SExpect

instance Ord wi => Semigroup (SExpect wi ri tk) where
  s1 <> s2 =
    SExpect (seExpects s1 <> seExpects s2) (seResults s1 <> seResults s2)

instance Ord wi => Monoid (SExpect wi ri tk) where
  mempty = SExpect mempty mempty

data SFStatus e = Expecting e | NotExpecting deriving (Show, Read, Generic, Eq, Ord)
type SFStatusFull wo tk = SFStatus (OSet wo, Task tk)

data SFError =
    SFEAlreadyFinished
  | SFEInvalidPrecondition {
        sfePreExpect :: !(SFStatus ())
      , sfePreActual :: !(SFStatus ())
    }
  deriving (Show, Read, Generic, Eq, Ord)

sCheckStatus
  :: (HasCallStack, Ord wi, Ord wo)
  => wi
  -> wo
  -> Lens' s (SFuture wo r)
  -> Lens' s (SExpect wi r tk)
  -> s
  -> SFStatusFull wo tk
sCheckStatus sfi sei lsf lse s =
  case (s ^. lsf, s ^. lse . _seExpects . at sfi) of
    (SFResult  _      , Just _ ) -> error "SFuture result but SExpect expects"
    (SFWaiting waiting, Just lt) -> if waiting ^. contains sei
      then Expecting (waiting, lt)
      else error "SFuture not waiting but SExpect expects"
    (SFResult  _      , Nothing) -> NotExpecting
    (SFWaiting waiting, Nothing) -> if waiting ^. contains sei
      then error "SFuture waiting but SExpect not expects"
      else NotExpecting

sExpectFuture
  :: (Ord wi, Ord wo)
  => TickDelta
  -> tk
  -> wi
  -> wo
  -> Lens' s (SFuture wo r)
  -> Lens' s (SExpect wi r tk)
  -> Lens' s (Schedule tk)
  -> s
  -> Either SFError s
sExpectFuture d t sfi sei lsf lse lsch s0 = case status of
  Expecting _  -> Left $ SFEInvalidPrecondition NotExpecting (Expecting ())
  NotExpecting -> case s0 ^. lsf of
    SFWaiting sfWaiting -> do
      let (lt, s1) = s0 & lsch %%~ after d t
      Right
        $ s1
        -- SExpect add expecting, set timeout
        & (lse . _seExpects . at sfi ?~ lt)
        -- SFuture add sfWaiting
        & (lsf .~ SFWaiting (sfWaiting & contains sei .~ True))
    SFResult r -> do
      -- SExpect add result to seResults
      Right $ s0 & lse . _seResults . at sfi ?~ GotResult r
  where status = sCheckStatus sfi sei lsf lse s0

sExpectCancel
  :: (Ord wi, Ord wo)
  => wi
  -> wo
  -> Lens' s (SFuture wo r)
  -> Lens' s (SExpect wi r tk)
  -> Lens' s (Schedule tk)
  -> s
  -> Either SFError s
sExpectCancel sfi sei lsf lse lsch s0 = case status of
  NotExpecting -> Left $ SFEInvalidPrecondition (Expecting ()) NotExpecting
  Expecting (sfWaiting, lt) -> do
    Right
      $ s0
      -- SExpect drop expects, clear timeout
      & (lsch %~ (snd . cancel_ lt))
      & (lse . _seExpects . at sfi .~ Nothing)
      -- SFuture drop sfWaiting
      & (lsf .~ SFWaiting (sfWaiting & contains sei .~ False))
  where status = sCheckStatus sfi sei lsf lse s0

sExpectTimeout
  :: (HasCallStack, Ord wi, Ord wo)
  => tk
  -> wi
  -> wo
  -> Lens' s (SFuture wo r)
  -> Lens' s (SExpect wi r tk)
  -> Lens' s (Schedule tk)
  -> s
  -> Either SFError s
sExpectTimeout tk sfi sei lsf lse lsch s0 = case status of
  NotExpecting      -> Left $ SFEInvalidPrecondition (Expecting ()) NotExpecting
  Expecting (_, lt) -> do
    -- SExcept add (TimedOut tick) result
    let s1 = s0 & lse . _seResults . at sfi %~ \case
          Just _  -> error "SExpect expects but also results"
          Nothing -> Just (TimedOut tk)
    sExpectCancel sfi sei lsf lse lsch s1
  where status = sCheckStatus sfi sei lsf lse s0

sFutureResult
  :: (Ord wi, Ord wo)
  => r
  -> wi
  -> Lens' s (SFuture wo r)
  -> IndexedTraversal' wo s (SExpect wi r tk)
  -> Lens' s (Schedule tk)
  -> s
  -> Either SFError s
sFutureResult r sfi lsf lsse lsch s0 = do
  (waiting, s1) <- getCompose $ s0 & lsf %%~ \case
    SFResult  _ -> Compose (Left SFEAlreadyFinished)
    SFWaiting w -> Compose (Right (w, SFResult r))
  let sch0       = s1 ^. lsch
  let (s2, sch1) = f waiting s1 sch0
  let s3         = s2 & lsch .~ sch1
  Right s3
 where
  -- TODO: iterate in order of w, not the traversal
  f w s = runState $ s & lsse . indices (`S.member` w) %%~ g
  g se = do
    let SExpect {..} = se
    -- SExpect drop expects, clear timeout
    let (lt', seExpects') = seExpects & at sfi %%~ (, Nothing)
        lt = fromJustNote "SFuture idx not found in SExpect expects" lt'
    state $ cancel_ lt
    let seResults' = seResults & at sfi ?~ GotResult r
    -- SExpect add result to seResults
    pure $ se { seExpects = seExpects', seResults = seResults' }
