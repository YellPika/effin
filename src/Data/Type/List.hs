{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Type.List (
    List (..), type (++),
    Is, InstanceOf,
    Inclusive,
    Size, KnownList (..),
    Index, Member (..),
    delete, undelete, prepend, append, decrease
) where

import Control.Arrow (left)
import Data.Proxy (Proxy (..))
import Data.Type.Bool (If)
import Data.Type.Equality ((:~:) (..), TestEquality (..))

infixr 5 :+, :-

-- | Type level list with explicit removals.
data List a
    = a :+ List a -- ^ Prepends an element.
    | a :- List a -- ^ Deletes the first occurence of an element.
    | Nil           -- ^ An empty list.

-- | Appends two lists.
type family l ++ m :: List (* -> *) where
    Nil ++ m = m
    (f :+ l) ++ m = f :+ (l ++ m)
    (f :- l) ++ m = f :- (l ++ m)

-- | Returns a boolean value indicating whether @f@ belongs to the group of
-- effects identified by @name@. This allows `MemberEffect` to infer associated
-- types for arbitrary effects.
type family Is name (f :: * -> *) :: Bool

type InstanceOf name l = NthInstanceOf name Z l

type family NthInstanceOf name n l where
    NthInstanceOf name Z     (f :+ l) = If (Is name f) f (NthInstanceOf name Z l)
    NthInstanceOf name (S n) (f :+ l) = NthInstanceOf name (If (Is name f) (S n) n) l
    NthInstanceOf name n     (f :- l) = NthInstanceOf name (If (Is name f) (S n) n) l

-- | Describes `List`s which do not contain deletions.
class KnownList l => Inclusive l
instance Inclusive Nil
instance Inclusive l => Inclusive (f :+ l)

data Size :: List (* -> *) -> * where
    Zero :: Size Nil
    SuccI :: Size l -> Size (f :+ l)
    SuccD :: Size l -> Size (f :- l)

-- | Describes a type level list who's size is known at compile time.
class KnownList l where
    size :: Size l

    -- Prevents the `Minimal Complete Definition` box from showing.
    size = undefined

instance KnownList Nil where
    size = Zero

instance KnownList l => KnownList (f :+ l) where
    size = SuccI size

instance KnownList l => KnownList (f :- l) where
    size = SuccD size

data Index :: List (* -> *) -> (* -> *) -> * where
    Head :: Index (f :+ l) f
    TailI :: Index l f -> Index (g :+ l) f
    TailD :: Index l f -> Index (g :- l) f

delete :: Index l f -> Index (g :- l) f
delete = TailD

undelete :: Index (g :- l) f -> Index l f
undelete (TailD i) = i

prepend :: Size l -> Index m f -> Index (l ++ m) f
prepend Zero i = i
prepend (SuccI n) i = TailI (prepend n i)
prepend (SuccD n) i = TailD (prepend n i)

append :: Index l f -> proxy m -> Index (l ++ m) f
append Head _ = Head
append (TailI i) p = TailI (append i p)
append (TailD i) p = TailD (append i p)

decrease :: Index (l ++ m) f -> Size l -> Either (Index l f) (Index m f)
decrease i Zero = Right i
decrease Head (SuccI _) = Left Head
decrease (TailI i) (SuccI n) = left TailI (decrease i n)
decrease (TailD i) (SuccD n) = left TailD (decrease i n)
decrease _ _ = error "Unreachable"

instance TestEquality (Index l) where
    testEquality Head Head = Just Refl
    testEquality (TailI i) (TailI j) = testEquality i j
    testEquality (TailD i) (TailD j) = testEquality i j
    testEquality _ _ = Nothing

-- | A constraint requiring the membership of @f@ in @l@.
class MemberAt f l (IndexOf f l) => Member f l where
    index :: Index l f
    index = undefined

instance MemberAt f l (IndexOf f l) => Member f l where
    index = indexAt (Proxy :: Proxy (IndexOf f l))

class MemberAt f l (n :: N) where
    indexAt :: proxy n -> Index l f

instance MemberAt f (f :+ l) Z where
    indexAt _ = Head

instance MemberAt f l n => MemberAt f (g :+ l) (S n) where
    indexAt _ = TailI $ indexAt (Proxy :: Proxy n)

instance MemberAt f l n => MemberAt f (g :- l) (S n) where
    indexAt _ = TailD $ indexAt (Proxy :: Proxy n)

type IndexOf f l = NthIndexOf Z f l

type family NthIndexOf n (f :: * -> *) l where
    NthIndexOf Z     f (f :+ l) = Z
    NthIndexOf (S n) f (f :+ l) = S (NthIndexOf n f l)
    NthIndexOf n     f (g :+ l) = S (NthIndexOf n f l)
    NthIndexOf n     f (f :- l) = S (NthIndexOf (S n) f l)
    NthIndexOf n     f (g :- l) = S (NthIndexOf n f l)

data N = Z | S N
