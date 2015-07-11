{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Index (
    Index,
    zero, index,
    absurd, trivial,
    swap, rotate,
    push, pop,
    disable, enable,
    conceal, reveal,
    prepend, append, split
) where

import Data.Type.Row
import Data.Type.Nat

import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (..), TestEquality (..))
import Unsafe.Coerce (unsafeCoerce)

newtype Index (l :: Row k) (e :: k) = Index Integer
  deriving Show

instance TestEquality (Index l) where
    testEquality (Index i) (Index j)
        | i == j = Just (unsafeCoerce Refl)
        | otherwise = Nothing

zero :: Index (e ':+ l) e
zero = Index 0

index :: forall e l. Member e l => Index l e
index = Index $ natVal (Proxy :: Proxy (IndexOf e l))

absurd :: Index 'Nil e -> a
absurd (Index i) = i `seq` error "absurd Index"

trivial :: Index (e ':+ 'Nil) f -> f :~: e
trivial (Index i)
    | i == 0 = unsafeCoerce Refl
    | otherwise = error "non-trivial Index"

size :: forall l proxy. KnownLength l => proxy l -> Integer
size _ = natVal (Proxy :: Proxy (Length l))

push :: Index l e -> Index (f ':+ l) e
push (Index i) = Index (i + 1)

pop :: Index (f ':+ l) e -> Index l e
pop (Index i) = Index (i - 1)

disable :: Index l e -> Index (f ':- l) e
disable (Index i) = Index (i + 1)

enable :: Index (f ':- l) e -> Index l e
enable (Index i) = Index (i - 1)

conceal :: forall e f l. Member f l => Index (f ':+ l) e -> Index l e
conceal (Index i)
    | i == 0 = Index j
    | otherwise = Index (i - 1)
  where
    Index j = index :: Index l f

reveal :: forall e f l. Member f l => Index l e -> Index (f ':+ l) e
reveal (Index i)
    | i == j = Index 0
    | otherwise = Index (i + 1)
  where
    Index j = index :: Index l f

swap :: Index (e ':+ f ':+ l) g -> Index (f ':+ e ':+ l) g
swap (Index i)
    | i == 0 = Index 1
    | i == 1 = Index 0
    | otherwise = Index i

rotate :: Index (e ':+ f ':+ g ':+ l) h -> Index (f ':+ g ':+ e ':+ l) h
rotate (Index i)
    | i == 0 = Index 2
    | i == 1 = Index 0
    | i == 2 = Index 1
    | otherwise = Index i

prepend :: KnownLength l => proxy l -> Index m e -> Index (l :++ m) e
prepend p (Index i) = Index (i + size p)

append :: Index l e -> proxy m -> Index (l :++ m) e
append (Index i) _ = Index i

split :: forall e l m. KnownLength l => Index (l :++ m) e -> Either (Index l e) (Index m e)
split (Index i)
    | i < n = Left (Index i)
    | otherwise = Right (Index (i - n))
  where
    n = size (Proxy :: Proxy l)
