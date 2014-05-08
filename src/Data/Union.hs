{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Union (
    Union,
    inject, injectAt,
    project, projectAt,
    reduce,
    withUnion, withUnionIndex,
    absurdUnion
) where

import Data.Member (Member (..), Index (..), withMember)

data Union es a where
    Union :: Functor e => Index e es -> e a -> Union es a

instance Functor (Union es) where
    fmap f (Union n x) = Union n (fmap f x)

inject :: (Functor e, Member e es) => e a -> Union es a
inject = Union index

injectAt :: Functor e => Index e es -> e a -> Union es a
injectAt = Union

project :: (Functor e, Member e es) => Union es a -> Maybe (e a)
project = projectAt index

projectAt :: Index e es -> Union es a -> Maybe (e a)
projectAt i (Union j x) = project' j i x

project' :: Index e es -> Index f es -> e a -> Maybe (f a)
project' Zero Zero = Just
project' (Succ n) (Succ m) = project' n m
project' _ _ = const Nothing

reduce :: Union (e ': es) a -> Either (Union es a) (e a)
reduce (Union Zero x) = Right x
reduce (Union (Succ n) x) = Left (Union n x)

withUnion :: (forall e. (Functor e, Member e es) => e a -> r) -> Union es a -> r
withUnion f (Union i x) = withMember (f x) i

withUnionIndex :: (forall e. Functor e => Index e es -> e a -> r) -> Union es a -> r
withUnionIndex f (Union i x) = f i x

absurdUnion :: Union '[] a -> b
absurdUnion (Union i _) = (case i of)
