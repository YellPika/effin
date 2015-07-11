{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Type.Nat (
    Nat (..), KnownNat (..)
) where

import Data.Proxy (Proxy (..))

data Nat = Zero | Succ Nat

class KnownNat (n :: Nat) where
    natVal :: proxy n -> Integer

instance KnownNat 'Zero where
    natVal _ = 0

instance KnownNat n => KnownNat ('Succ n) where
    natVal _ = 1 + natVal (Proxy :: Proxy n)
