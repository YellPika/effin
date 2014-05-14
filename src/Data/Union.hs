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
    unwrap, wrap,
    inject, project,
    split, combine,
    reduce, extend,
    flatten, unflatten,
    withUnion, absurdUnion,

    KnownList, type (++)
) where

import Data.Proxy (Proxy (..))
import Unsafe.Coerce (unsafeCoerce)

-- | Represents a union of the list of type constructors in @es@ parameterized
-- by @a@. As an effect, it represents the union of each type constructor's
-- corresponding effect.
data Union es a where
    Union :: Functor e => Index e es -> e a -> Union es a

instance Functor (Union es) where
    fmap f (Union i x) = Union i (fmap f x)

unwrap :: Union '[e] a -> e a
unwrap = either absurdUnion id . reduce

wrap :: Functor e => e a -> Union '[e] a
wrap = inject

inject :: Member e es => e a -> Union es a
inject = Union index

project :: forall a e es. Member e es => Union es a -> Maybe (e a)
project (Union (Index i) x)
    | i == j = Just (unsafeCoerce x)
    | otherwise = Nothing
  where
    Index j = index :: Index e es

split :: forall a es fs. KnownList es => Union (es ++ fs) a -> Either (Union fs a) (Union es a)
split (Union (Index i) x)
    | i < n = Right $ Union (Index i) x
    | otherwise = Left $ Union (Index (i - n)) x
  where
    Size n = size :: Size es

combine :: forall a es fs. KnownList es => Either (Union fs a) (Union es a) -> Union (es ++ fs) a
combine (Right (Union (Index i) x)) = Union (Index i) x
combine (Left (Union (Index i) x)) = Union (Index (i + n)) x
  where
    Size n = size :: Size es

extend :: Functor e => Either (Union es a) (e a) -> Union (e ': es) a
extend = combine . fmap wrap

reduce :: Union (e ': es) a -> Either (Union es a) (e a)
reduce = fmap unwrap . split

flatten :: KnownList es => Union (Union es ': fs) a -> Union (es ++ fs) a
flatten = combine . reduce

unflatten :: KnownList es => Union (es ++ fs) a -> Union (Union es ': fs) a
unflatten = extend . split

withUnion :: (forall e. Member e es => e a -> r) -> Union es a -> r
withUnion f (Union i x) = withIndex (f x) (\Proxy -> i)

absurdUnion :: Union '[] a -> b
absurdUnion _ = error "absurdUnion"

-- | A constraint that requires that the type constructor @t :: * -> *@ is a
-- member of the list of types @ts :: [* -> *]@.
class (Functor e, Member' e es (IndexOf e es)) => Member e es where
    index :: Index e es

    -- Default definition hides "Minimal complete definition" section.
    index = undefined

instance (Functor e, Member' e es (IndexOf e es)) => Member e es where
    index = index' (Proxy :: Proxy (IndexOf e es))

class Member' e es (n :: N) where
    index' :: Proxy n -> Index e es

instance Member' e (e ': es) Z where
    index' Proxy = Index 0

instance (Member' e es n, IndexOf e (f ': es) ~ S n) => Member' e (f ': es) (S n) where
    index' Proxy = Index (i + 1)
      where
        Index i = index' (Proxy :: Proxy n) :: Index e es

newtype Index (e :: * -> *) (es :: [* -> *]) = Index Integer

withIndex :: (Member' e es (IndexOf e es) => r) -> (Proxy (IndexOf e es) -> Index e es) -> r
withIndex = unsafeCoerce

data N = Z | S N

type family IndexOf (e :: * -> *) es where
    IndexOf e (e ': es) = Z
    IndexOf e (f ': es) = S (IndexOf e es)

newtype Size (es :: [* -> *]) = Size Integer

-- | A 'known list' is a type level list who's size is known at compile time.
class KnownList es where
    size :: Size es

instance KnownList '[] where
    size = Size 0

instance KnownList es => KnownList (e ': es) where
    size = let Size n = size :: Size es in Size (n + 1)

-- | Type level list append function.
type family es ++ fs :: [* -> *] where
    '[] ++ fs = fs
    (e ': es) ++ fs = e ': (es ++ fs)
