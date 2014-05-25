{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

#ifdef MTL
import Control.Monad.Trans (MonadIO (..))

instance EffectLift IO l => MonadIO (Effect l) where
    liftIO = lift
#endif

-- | An effect described by a monad.
newtype Lift m a = Lift { unLift :: m a }

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
    (\(Lift m) k -> return $ m >>= runEffect . k)
