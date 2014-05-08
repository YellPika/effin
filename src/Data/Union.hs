{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Union (
    Union, Member,
    inject, project,
    reduce, withUnion,
    absurdUnion
) where

import Unsafe.Coerce (unsafeCoerce)

-- Union -----------------------------------------------------------------------
data Union es a where
    Union :: Functor e => Index e es -> e a -> Union es a

instance Functor (Union es) where
    fmap f (Union i x) = Union i (fmap f x)

inject :: Member e es => e a -> Union es a
inject = Union index

project :: forall a e es. Member e es => Union es a -> Maybe (e a)
project (Union (Index i) x)
    | i == j = Just (unsafeCoerce x)
    | otherwise = Nothing
  where
    Index j = index :: Index e es

reduce :: Union (e ': es) a -> Either (Union es a) (e a)
reduce (Union (Index 0) x) = Right (unsafeCoerce x)
reduce (Union (Index n) x) = Left (Union (Index (n - 1)) x)

withUnion :: (forall e. Member e es => e a -> r) -> Union es a -> r
withUnion f (Union i x) = withIndex (f x) i

absurdUnion :: Union '[] a -> b
absurdUnion _ = error "absurdUnion"

-- Membership ------------------------------------------------------------------

-- | A constraint that requires that the type constructor @t :: * -> *@ is a
-- member of the list of types @ts :: [* -> *]@.
class (Functor t, Member' t ts (IndexOf t ts)) => Member t ts
instance (Functor t, Member' t ts (IndexOf t ts)) => Member t ts

class n ~ IndexOf e es => Member' e es n where
    index :: Index e es

instance Member' e (e ': es) Z where
    index = Index 0

instance (Member' e es n, IndexOf e (f ': es) ~ S n) => Member' e (f ': es) (S n) where
    index = incr index

-- Member Indices --------------------------------------------------------------
data Index (e :: * -> *) (es :: [* -> *]) = Index Integer

incr :: Index e es -> Index e (f ': es)
incr (Index i) = Index (i + 1)

withIndex :: Functor e => (Member e es => r) -> Index e es -> r
withIndex = unsafeCoerce

-- Type Level Indices ----------------------------------------------------------
data N = Z | S N

type family IndexOf (t :: * -> *) ts where
    IndexOf t (t ': ts) = Z
    IndexOf t (u ': ts) = S (IndexOf t ts)
