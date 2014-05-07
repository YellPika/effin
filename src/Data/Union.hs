{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Union (
    Union, Member,
    inject, project,
    reduce, withUnion,
    absurdUnion
) where

-- Union -----------------------------------------------------------------------
data Union es a where
    Union :: Index e es -> e a -> Union es a

inject :: Member e es => e a -> Union es a
inject = Union index

project :: Member e es => Union es a -> Maybe (e a)
project (Union i x) = project' index i x
  where
    project' :: Index e es -> Index f es -> f a -> Maybe (e a)
    project' Zero Zero = Just
    project' (Succ n) (Succ m) = project' n m
    project' _ _ = const Nothing

reduce :: Union (e ': es) a -> Either (Union es a) (e a)
reduce (Union Zero x) = Right x
reduce (Union (Succ n) x) = Left (Union n x)

withUnion :: (forall e. Member e es => e a -> r) -> Union es a -> r
withUnion f (Union i x) = withIndex (f x) i

absurdUnion :: Union '[] a -> b
absurdUnion (Union i _) = (case i of)

-- Membership ------------------------------------------------------------------

-- | A constraint that requires that the type constructor @t :: * -> *@ is a
-- member of the list of types @ts :: [* -> *]@.
class Member' t ts (IndexOf t ts) => Member t ts
instance Member' t ts (IndexOf t ts) => Member t ts

class n ~ IndexOf e es => Member' e es n where
    index :: Index e es

instance Member' e (e ': es) Z where
    index = Zero

instance (Member' e es n, IndexOf e (f ': es) ~ S n) => Member' e (f ': es) (S n) where
    index = Succ index

-- Member Indices --------------------------------------------------------------
data Index e es where
    Zero :: Index e (e ': es)
    Succ :: IndexOf e (f ': es) ~ S (IndexOf e es) => Index e es -> Index e (f ': es)

withIndex :: (Member e es => r) -> Index e es -> r
withIndex x Zero = x
withIndex x (Succ n) = withIndex x n

-- Type Level Indices ----------------------------------------------------------
data N = Z | S N

type family IndexOf (t :: * -> *) ts where
    IndexOf t (t ': ts) = Z
    IndexOf t (u ': ts) = S (IndexOf t ts)
