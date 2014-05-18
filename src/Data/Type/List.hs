{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Type.List (
    List (..), (:++),
    Length, KnownLength,
    IndexOf, Member,
    Inclusive,
    Is, InstanceOf
) where

import Data.Type.Nat
import Data.Type.Bool (If)

infixr 5 :+, :-, :++

-- | A type level list with explicit removals.
data List a
    = Nil         -- ^ The empty list.
    | a :+ List a -- ^ Prepends an element (cons).
    | a :- List a -- ^ Deletes the first instance an element.

-- | Appends two type level lists.
type family l :++ m where
    Nil :++ l = l
    (e :+ l) :++ m = e :+ l :++ m
    (e :- l) :++ m = e :- l :++ m

-- | Returns the length of the list @l@.
type family Length l where
    Length Nil = Zero
    Length (h :+ t) = Succ (Length t)
    Length (h :- t) = Succ (Length t)

-- | The class of lists with statically known lengths.
class KnownNat (Length l) => KnownLength l
instance KnownNat (Length l) => KnownLength l

-- | Returns the index of the first instance of @e@ in @l@.
type IndexOf e l = NthIndexOf Zero e l

type family NthIndexOf n e l where
    NthIndexOf Zero     e (e :+ l) = Zero
    NthIndexOf (Succ n) e (e :+ l) = Succ (NthIndexOf n e l)
    NthIndexOf n        e (f :+ l) = Succ (NthIndexOf n e l)
    NthIndexOf n        e (e :- l) = Succ (NthIndexOf (Succ n) e l)
    NthIndexOf n        e (f :- l) = Succ (NthIndexOf n e l)

-- | A constraint specifying that @e@ is a member of @l@.
class KnownNat (IndexOf e l) => Member e l
instance KnownNat (IndexOf e l) => Member e l

-- | The class of type level lists that do not contain deletions (`:-`).
class KnownLength l => Inclusive l
instance Inclusive Nil
instance Inclusive l => Inclusive (e :+ l)

-- | Returns a boolean value indicating whether @f@ belongs to the group of
-- effects identified by @name@. This allows `MemberEffect` to infer associated
-- types for arbitrary effects.
type family Is name (f :: * -> *) :: Bool

type InstanceOf name l = NthInstanceOf name Zero l

type family NthInstanceOf name n l where
    NthInstanceOf name Zero     (f :+ l) = If (Is name f) f (NthInstanceOf name Zero l)
    NthInstanceOf name (Succ n) (f :+ l) = NthInstanceOf name (If (Is name f) (Succ n) n) l
    NthInstanceOf name n        (f :- l) = NthInstanceOf name (If (Is name f) (Succ n) n) l
