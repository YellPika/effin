{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Effect.Lift where

import Control.Monad (join, liftM)
import Control.Monad.Effect (Effect, Member, send, handle, eliminate, emptyRelay)

data Lift m a = Lift { unLift :: m a }

instance Monad m => Functor (Lift m) where
    fmap f = Lift . liftM f . unLift

type EffectLift m es = (Member (Lift m) es, m ~ LiftType es, Monad m)
type family LiftType es where
    LiftType (Lift m ': es) = m
    LiftType (e ': es) = LiftType es

lift :: EffectLift m es => m a -> Effect es a
lift = send . Lift . liftM return

runLift :: Monad m => Effect '[Lift m] a -> m a
runLift =
    handle return
    $ eliminate (join . unLift)
    $ emptyRelay
