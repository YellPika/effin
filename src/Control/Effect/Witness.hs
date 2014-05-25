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
    Token, newToken
) where

import Control.Monad.Effect
import Data.Type.Equality ((:~:) (..), TestEquality (..))
import Data.Unique (Unique, newUnique)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

-- | A unique identifier associated with a type @a@.
-- If two tokens are equal, then so are their associated types.
-- Use `testEquality` to safely cast between types.
newtype Token s a = Token Unique
  deriving Eq

instance TestEquality (Token s) where
    testEquality (Token i) (Token j)
        | i == j = Just unsafeRefl
        | otherwise = Nothing

unsafeRefl :: a :~: b
unsafeRefl = unsafeCoerce Refl

-- | An effect describing the generation of unique identifiers.
data Witness s a where
    NewToken :: Witness s (Token s a)

type instance Is Witness f = IsWitness f

type family IsWitness f where
    IsWitness (Witness s) = True
    IsWitness f = False

class MemberEffect Witness (Witness s) l => EffectWitness s l
instance MemberEffect Witness (Witness s) l => EffectWitness s l

type family WitnessType l where
    WitnessType (Witness s ': l) = s
    WitnessType (e ': l) = WitnessType l

-- | Generates a new, unique `Token`.
newToken :: EffectWitness s l => Effect l (Token s a)
newToken = send NewToken

-- | Completely handles a `Witness` effect. The Rank-2 quantifier ensures that
-- unique identifiers cannot escape the context in which they were created.
runWitness :: (forall s. Effect (Witness s :+ l) a) -> Effect l a
runWitness effect = run effect
  where
    run = eliminate return $ \NewToken k ->
        k $ Token $ unsafePerformIO newUnique
