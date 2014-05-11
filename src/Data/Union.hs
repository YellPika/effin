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
    reduce, flatten,
    withUnion, absurdUnion,

    KnownList, type (++)
) where

import Data.Proxy (Proxy (..))
import Unsafe.Coerce (unsafeCoerce)

-- Union -----------------------------------------------------------------------

-- | Represents a union of the list of type constructors in @es@ parameterized
-- by @a@. As an effect, it represents the union of each type constructor's
-- corresponding effect.
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

flatten :: KnownList es => Union (Union es ': fs) a -> Union (es ++ fs) a
flatten = flatten' size . reduce
  where
    flatten' :: Size es -> Either (Union fs a) (Union es a) -> Union (es ++ fs) a
    flatten' _ (Right (Union (Index i) x)) = Union (Index i) x
    flatten' (Size n) (Left (Union (Index i) x)) = Union (Index (n + i)) x

withUnion :: (forall e. Member e es => e a -> r) -> Union es a -> r
withUnion f (Union i x) = withIndex (f x) (\Proxy -> i)

absurdUnion :: Union '[] a -> b
absurdUnion _ = error "absurdUnion"

-- Membership ------------------------------------------------------------------

-- | A constraint that requires that the type constructor @t :: * -> *@ is a
-- member of the list of types @ts :: [* -> *]@.
class (Functor t, Member' t ts (IndexOf t ts)) => Member t ts where
    index :: Index t ts

    -- Default definition hides "Minimal complete definition" section.
    index = undefined

instance (Functor t, Member' t ts (IndexOf t ts)) => Member t ts where
    index = index' (Proxy :: Proxy (IndexOf t ts))

class Member' e es (n :: N) where
    index' :: Proxy n -> Index e es

instance Member' e (e ': es) Z where
    index' _ = Index 0

instance (Member' e es n, IndexOf e (f ': es) ~ S n) => Member' e (f ': es) (S n) where
    index' p = incr (index' (decr p))
      where
        incr :: Index e es -> Index e (f ': es)
        incr (Index i) = Index (i + 1)

        decr :: Proxy (S n) -> Proxy n
        decr Proxy = Proxy

newtype Index (e :: * -> *) (es :: [* -> *]) = Index Integer

withIndex :: (Member' e es (IndexOf e es) => r) -> (Proxy (IndexOf e es) -> Index e es) -> r
withIndex = unsafeCoerce

-- Type Level Indices ----------------------------------------------------------
data N = Z | S N

type family IndexOf (t :: * -> *) ts where
    IndexOf t (t ': ts) = Z
    IndexOf t (u ': ts) = S (IndexOf t ts)

-- Type Level Lists ------------------------------------------------------------
newtype Size (es :: [* -> *]) = Size Integer

-- | A 'known list' is a type level list who's size is known at compile time.
class KnownList es where
    size :: Size es

instance KnownList '[] where
    size = Size 0

instance KnownList es => KnownList (e ': es) where
    size = incr size
      where
        incr :: Size es -> Size (e ': es)
        incr (Size n) = Size (n + 1)

-- | Type level list append function.
type family es ++ fs :: [* -> *] where
    '[] ++ fs = fs
    (e ': es) ++ fs = e ': (es ++ fs)
