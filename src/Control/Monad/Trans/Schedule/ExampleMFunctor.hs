{-# LANGUAGE
    RankNTypes
  , GeneralizedNewtypeDeriving
  #-}

module Control.Monad.Trans.Schedule.ExampleMFunctor (
    Ctx(..)
  , F
  , hoist'
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Morph

import Control.Monad.Trans.Schedule -- for haddock

{-| A simpler version of 'ScheduleT' that has the "same shape" in a way that is
relevant to what we're trying to accomplish, which is to define 'hoist' for it.

It is effecively a 'StateT' whose state type is the same as[*] the monad type
that it's transforming.

- [*] or more generally, a (covariant) functor of - e.g. see 'F'.
-}
newtype Ctx m a = Ctx { getC :: StateT (F m) m a }
  deriving (Functor, Applicative, Monad)

-- | A type that represents the state type, that is dependent on @m@. This could
-- be any covariant functor of @m@ and everything we say in the rest of this file
-- would still be true.
type F m = m ()

instance MonadTrans Ctx where
  lift = Ctx . lift

-- | This is based on 'hoist'' which is 'undefined'.
instance MFunctor Ctx where
  hoist morph = Ctx . StateT . hoist' morph . runStateT . getC

{-| To 'hoist' something, we apply a morphism @m -> n@ to the inner monad @m@.
This is usually not so hard. But with 'Ctx', @m@ also determines the state type
(roughly @m -> m@) so we must also transform that. In other words we need to
somehow transform @m -> m@ to @n -> n@, using only a morphism @m -> n@. This is
probably impossible:

@(->)@ is a 'Data.Profunctor' - a functor of two arguments, contravariant in
the first and covariant in the second:

> dimap :: (a -> b) -> (c -> d) -> p b c -> p a d

To transform @p m m@ into @p n n@, we need both @m -> n@ and @n -> m@:

> dimap :: (n -> m) -> (m -> n) -> p m m -> p n n -- substitute vars

An alternative is to try to treat @m -> m@ as a functor (of one argument) which
is both covariant and contravariant in that argument. But it is a known result
that this would be useless, see 'Data.Functor.Contravariant.phantom'.

If this was too abstract, you can look at the source code of our attempt to
actually implement 'hoist''.

-}

hoist' :: Monad m
  => (forall a. m a -> n a)
  -> (F m -> m (b, F m))
  -> (F n -> n (b, F n))
hoist' morph s0 s = morph $ do
    let s' = morph `contramorph` s
    (a, t') <- s0 s'
    let t = morph `fmorph` t'
    return (a, t)

contramorph :: (forall a. m a -> n a) -> F n -> F m
contramorph _ _ = undefined -- can't do it :(

fmorph :: (forall a. m a -> n a) -> F m -> F n
fmorph m s = m s
