{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Data.Union (
    Union,
    wrap, unwrap,
    inject, project,
    split, combine,
    reduce, expand,
    flatten, unflatten,
    remove, absurdUnion
) where

import Data.Type.List
import Control.Arrow (left)
import Data.Maybe (fromJust)
import Data.Type.Equality ((:~:) (..), testEquality)
import Data.Proxy (Proxy (..))

data Union l a where
    Union :: Functor f => Index l f -> f a -> Union l a

instance Functor (Union l) where
    fmap f (Union i x) = Union i (fmap f x)

wrap :: Functor f => f a -> Union (f +: Nil) a
wrap = inject

unwrap :: Union (f +: Nil) a -> f a
unwrap = fromJust . project

inject :: (Functor f, Member f l) => f a -> Union l a
inject = Union index

project :: Member f l => Union l a -> Maybe (f a)
project (Union i x) = project' i index x

project' :: Index l f -> Index l g -> f a -> Maybe (g a)
project' i j x = fmap (\Refl -> x) (testEquality i j)

split :: KnownList l => Union (l ++ m) a -> Either (Union l a) (Union m a)
split (Union i x) =
    case decrease i size of
        Left j -> Left (Union j x)
        Right j -> Right (Union j x)

combine :: Inclusive l => Either (Union l a) (Union m a) -> Union (l ++ m) a
combine = combine' size Proxy

combine' :: Size l -> proxy m -> Either (Union l a) (Union m a) -> Union (l ++ m) a
combine' _ p (Left (Union i x)) = Union (append i p) x
combine' n _ (Right (Union i x)) = Union (prepend n i) x

reduce :: Union (f +: l) a -> Either (f a) (Union l a)
reduce = left unwrap . split

expand :: Functor f => Either (f a) (Union l a) -> Union (f +: l) a
expand = combine . left wrap

flatten :: Inclusive l => Union (Union l +: m) a -> Union (l ++ m) a
flatten = combine . reduce

unflatten :: KnownList l => Union (l ++ m) a -> Union (Union l +: m) a
unflatten = expand . split

remove :: Member f l => Union l a -> Either (f a) (Union (f -: l) a)
remove u@(Union i x) =
    case project u of
        Just x' -> Left x'
        Nothing -> Right (Union (delete i) x)

absurdUnion :: Union Nil a -> b
absurdUnion (Union i _) = (case i of)
