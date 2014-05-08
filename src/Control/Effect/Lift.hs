{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Effect.Lift where

import Control.Monad (join, liftM)
import Control.Monad.Effect (Effect, sendAt, handle, eliminate, emptyRelay)
import Data.Member (Member (..), Index)

-- | An effect described by a monad.
-- All monads are functors, but not all `Monad`s have `Functor` instances.
-- Since all effects must have `Functor` instances, this type ensures that
-- all monads can be used by providing a `Functor` instance derived from a
-- `Monad` instance.
data Lift m a = Lift { unLift :: m a }

instance Monad m => Functor (Lift m) where
    fmap f = Lift . liftM f . unLift

type EffectLift m es = (Member (Lift m) es, m ~ LiftType es, Monad m)
type family LiftType es where
    LiftType (Lift m ': es) = m
    LiftType (e ': es) = LiftType es

-- | Lifts a monadic value into an effect.
lift :: EffectLift m es => m a -> Effect es a
lift = liftAt index

liftAt :: Monad m => Index (Lift m) es -> m a -> Effect es a
liftAt i = sendAt i . Lift

-- | Converts a computation containing only monadic
-- effects into a monadic computation.
runLift :: Monad m => Effect '[Lift m] a -> m a
runLift =
    handle return
    $ eliminate (join . unLift)
    $ emptyRelay
