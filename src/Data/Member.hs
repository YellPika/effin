{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Member (
    Member (index), Index (..), withIndex
) where

import Unsafe.Coerce (unsafeCoerce)

-- | An index describing the position of a type within a type list.
data Index e es where
    Zero :: Index e (e ': es)
    Succ :: Index e es -> Index e (f ': es)

withIndex :: Index e es -> (Member e es => r) -> r
withIndex = flip unsafeCoerce

class Member' e es (IndexOf e es) => Member e es where
    index :: Index e es

instance Member' e es (IndexOf e es) => Member e es where
    index = index'

class n ~ IndexOf e es => Member' e es n where
    index' :: Index e es

instance Member' e (e ': es) Z where
    index' = Zero

instance (Member e es
         , S n ~ IndexOf e (f ': es)
         ) => Member' e (f ': es) (S n) where
    index' = Succ index

data N = Z | S N
type family IndexOf e es where
    IndexOf e (e ': es) = Z
    IndexOf e (f ': es) = S (IndexOf e es)
