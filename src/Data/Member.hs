{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Data.Member (
    Member (..), Index (..), withMember, initMember
) where

import Unsafe.Coerce (unsafeCoerce)

-- | An index describing the position of a type within a type list.
data Index e es where
    Zero :: Index e (e ': es)
    Succ :: Index e es -> Index e (f ': es)

withMember :: (Member e es => r) -> Index e es -> r
withMember = unsafeCoerce

initMember :: (Member e (e ': es) => r) -> r
initMember = flip withMember Zero

class Member e es where
    index :: Index e es

instance Member e es => Member e (f ': es) where
    index = Succ index
