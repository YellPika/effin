{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.Witness (
    EffectWitness, Witness, runWitness,
    Token, newToken
) where

import Control.Monad.Effect
import Data.Type.Equality ((:~:) (..), TestEquality (..))
import Data.Unique (Unique, newUnique)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

newtype Token s a = Token Unique

instance TestEquality (Token s) where
    testEquality (Token i) (Token j)
        | i == j = Just unsafeRefl
        | otherwise = Nothing

data Witness s a where
    Witness :: (Token s b -> a) -> Witness s a

instance Functor (Witness s) where
    fmap f (Witness g) = Witness (f . g)

class (Member (Witness s) es, s ~ WitnessType es) => EffectWitness s es
instance (Member (Witness s) es, s ~ WitnessType es) => EffectWitness s es

type family WitnessType es where
    WitnessType (Witness s ': es) = s
    WitnessType (e ': es) = WitnessType es

unsafeRefl :: a :~: b
unsafeRefl = unsafeCoerce Refl

newToken :: EffectWitness s es => Effect es (Token s a)
newToken = send (Witness id)

runWitness :: (forall s. Effect (Witness s ': es) a) -> Effect es a
runWitness effect = run effect
  where
    run =
        handle return
        $ eliminate (\(Witness k) -> k $ Token $ unsafePerformIO newUnique)
        $ defaultRelay
