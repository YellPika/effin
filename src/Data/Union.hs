{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Data.Union (
    Union, absurd,
    wrap, unwrap,
    inject, project,
    swap, rotate,
    push, pop,
    enable, disable,
    conceal, reveal,
    flatten, unflatten
) where

import Data.Index (Index, index)
import qualified Data.Index as Index

import Data.Type.Row
import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (..), apply, castWith, gcastWith, testEquality)

-- | Represents a union of the list of type constructors in @l@ parameterized
-- by @a@. As an effect, it represents the union of each type constructor's
-- corresponding effect.
data Union l a where
    Union :: Functor f => Index l f -> f a -> Union l a

instance Functor (Union l) where
    fmap f (Union i x) = Union i (fmap f x)

absurd :: Union Nil a -> b
absurd (Union i _) = Index.absurd i

wrap :: Functor f => f a -> Union (f :+ l) a
wrap = inject

unwrap :: Union (f :+ Nil) a -> f a
unwrap (Union i x) = gcastWith (Index.trivial i) x

inject :: (Functor f, Member f l) => f a -> Union l a
inject = Union index

project :: Member f l => Union l a -> Maybe (f a)
project (Union i x) = fmap (\refl -> castWith (apply refl Refl) x) mRefl
  where
    mRefl = testEquality i index

swap :: Union (f :+ g :+ l) a -> Union (g :+ f :+ l) a
swap (Union i x) = Union (Index.swap i) x

rotate :: Union (f :+ g :+ h :+ l) a -> Union (g :+ h :+ f :+ l) a
rotate (Union i x) = Union (Index.rotate i) x

push :: Union l a -> Union (f :+ l) a
push (Union i x) = Union (Index.push i) x

pop :: Union (f :+ l) a -> Either (f a) (Union l a)
pop u@(Union i x) =
    case project u of
        Just r -> Left r
        Nothing -> Right (Union (Index.pop i) x)

enable :: Union (f :- l) a -> Union l a
enable (Union i x) = Union (Index.enable i) x

disable :: Member f l => Union l a -> Either (f a) (Union (f :- l) a)
disable u@(Union i x) =
    case project u of
        Just r -> Left r
        Nothing -> Right (Union (Index.disable i) x)

conceal :: Member f l => Union (f :+ l) a -> Union l a
conceal (Union i x) = Union (Index.conceal i) x

reveal :: Member f l => Union l a -> Union (f :+ l) a
reveal (Union i x) = Union (Index.reveal i) x

flatten :: Inclusive l => Union (Union l :+ m) a -> Union (l :++ m) a
flatten = flatten' Proxy Proxy . pop
  where
    flatten' :: KnownLength l => proxy l -> proxy m -> Either (Union l a) (Union m a) -> Union (l :++ m) a
    flatten' _ p (Left (Union i x)) = Union (Index.append i p) x
    flatten' p _ (Right (Union i x)) = Union (Index.prepend p i) x

unflatten :: KnownLength l => Union (l :++ m) a -> Union (Union l :+ m) a
unflatten (Union i x) =
    case Index.split i of
        Left j -> Union Index.zero (Union j x)
        Right j -> Union (Index.push j) x
