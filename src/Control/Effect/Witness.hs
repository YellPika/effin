{-# LANGUAGE DataKinds #-}
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
    Token, newToken, tryCast
) where

import Control.Monad.Effect
import Data.Type.Equality ((:~:) (..))
import Data.Unique (Unique, newUnique)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

data Token s a = Token String Unique

instance Show (Token s a) where
    show (Token name _) = name

data Witness s a where
    Witness :: String -> (Token s b -> a) -> Witness s a

class (Member (Witness s) es, s ~ WitnessType es) => EffectWitness s es
instance (Member (Witness s) es, s ~ WitnessType es) => EffectWitness s es

type family WitnessType es where
    WitnessType (Witness s ': es) = s
    WitnessType (e ': es) = WitnessType es

unsafeRefl :: a :~: b
unsafeRefl = unsafeCoerce Refl

tryCast :: Token s a -> Token s b -> Maybe (a :~: b)
tryCast (Token _ i) (Token _ j)
    | i == j = Just unsafeRefl
    | otherwise = Nothing

newToken :: EffectWitness s es => String -> Effect es (Token s a)
newToken name = send (Witness name id)

runWitness :: (forall s. Effect (Witness s ': es) a) -> Effect es a
runWitness effect = run effect
  where
    run =
        handle return
        $ eliminate (\(Witness name k) -> k $ Token name $ unsafePerformIO newUnique)
        $ defaultRelay
