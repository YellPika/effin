{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Row (
    Row (..), (:++),
    Length, KnownLength,
    IndexOf, Member,
    Inclusive,
    Is, InstanceOf
) where

import Data.Type.Nat
import Data.Type.Bool (If)

infixr 5 :+, :-, :++

-- | A type level list with explicit removals.
data Row a
    = Nil        -- ^ The empty list.
    | a :+ Row a -- ^ Prepends an element (cons).
    | a :- Row a -- ^ Deletes the first instance an element.

-- | Appends two type level `Row`s.
type family l :++ m where
    'Nil :++ l = l
    (e ':+ l) :++ m = e ':+ l :++ m
    (e ':- l) :++ m = e ':- l :++ m

-- | Returns the length of the `Row` @l@.
type family Length l where
    Length 'Nil = 'Zero
    Length (h ':+ t) = 'Succ (Length t)
    Length (h ':- t) = 'Succ (Length t)

-- | The class of `Row`s with statically known lengths.
class KnownNat (Length l) => KnownLength l
instance KnownNat (Length l) => KnownLength l

-- | Returns the index of the first instance of @e@ in the `Row` @l@.
type IndexOf e l = NthIndexOf 'Zero e l

type family NthIndexOf n e l where
    NthIndexOf 'Zero     e (e ':+ l) = 'Zero
    NthIndexOf ('Succ n) e (e ':+ l) = 'Succ (NthIndexOf n e l)
    NthIndexOf n         e (f ':+ l) = 'Succ (NthIndexOf n e l)
    NthIndexOf n         e (e ':- l) = 'Succ (NthIndexOf ('Succ n) e l)
    NthIndexOf n         e (f ':- l) = 'Succ (NthIndexOf n e l)

-- | A constraint specifying that @e@ is a member of the `Row` @l@.
class KnownNat (IndexOf e l) => Member e l
instance KnownNat (IndexOf e l) => Member e l

-- | The class of `Row`s that do not contain deletions (`':-`).
class KnownLength l => Inclusive l
instance Inclusive 'Nil
instance Inclusive l => Inclusive (e ':+ l)

-- | Returns a boolean value indicating whether @f@ belongs to the group of
-- effects identified by @name@. This allows `MemberEffect` to infer the
-- associated types for arbitrary effects.
type family Is (name :: k) (f :: * -> *) :: Bool

type InstanceOf name l = InstanceOfNone name '[] l

-- Any instance of name in l but not in ex.
type family InstanceOfNone name ex l where
    InstanceOfNone name ex (f ':- l) = InstanceOfNone name (f ': ex) l
    InstanceOfNone name ex (f ':+ l) =
        If (Is name f)
            (If (Elem f ex) (InstanceOfNone name (Remove f ex) l) f)
            (InstanceOfNone name ex l)

type family Elem e l where
    Elem e '[] = 'False
    Elem e (e ': l) = 'True
    Elem e (f ': l) = Elem e l

type family Remove e l where
    Remove e (e ': l) = l
    Remove e (f ': l) = f ': Remove e l
