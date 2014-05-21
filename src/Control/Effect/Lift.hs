{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

#ifdef MTL
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

module Control.Effect.Lift (
    EffectLift, Lift (..), runLift, lift, liftEffect
) where

import Control.Monad.Effect
import Control.Monad (join, liftM)

#ifdef MTL
import Control.Monad.Trans (MonadIO (..))

instance EffectLift IO l => MonadIO (Effect l) where
    liftIO = lift
#endif

-- | An effect described by a monad.
-- All monads are functors, but not all `Monad`s have `Functor` instances.
-- By wrapping a monad in the `Lift` effect, all monads can be used without
-- having to provide a `Functor` instance for each one.
newtype Lift m a = Lift { unLift :: m a }

instance Monad m => Functor (Lift m) where
    fmap f = Lift . liftM f . unLift

type instance Is Lift f = IsLift f

type family IsLift f where
    IsLift (Lift m) = True
    IsLift f = False

class (Monad m, MemberEffect Lift (Lift m) l) => EffectLift m l
instance (Monad m, MemberEffect Lift (Lift m) l) => EffectLift m l

-- | Lifts a monadic value into an effect.
lift :: EffectLift m l => m a -> Effect l a
lift = send . Lift

-- | Lifts a monadic value into an effect.
liftEffect :: EffectLift m l => m (Effect l a) -> Effect l a
liftEffect = sendEffect . Lift

-- | Converts a computation containing only monadic
-- effects into a monadic computation.
runLift :: Monad m => Effect (Lift m :+ Nil) a -> m a
runLift = runEffect . eliminate
    (return . return)
    (return . join . liftM runEffect . unLift)
