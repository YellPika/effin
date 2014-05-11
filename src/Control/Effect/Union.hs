{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Effect.Union (
    EffectUnion, Union, runUnion, nest,
    KnownList, type (++),
) where

import Control.Monad.Effect
import Data.Union

type EffectUnion es fs = (KnownList es, Member (Union es) fs, es ~ UnionType fs)
type family UnionType fs where
    UnionType (Union es ': fs) = es
    UnionType (f ': fs) = UnionType fs

-- | Nests an effect with another.
nest :: EffectUnion es fs => Effect es a -> Effect fs a
nest =
    handle return
    $ relayUnion sendEffect

-- | Flattens a nested list of effects.
runUnion :: KnownList es => Effect (Union es ': fs) a -> Effect (es ++ fs) a
runUnion =
    handle return
    $ relayUnion (withUnion sendEffect . flatten)

relayUnion :: (Union es b -> b) -> Handler es b
relayUnion f = relay (f . inject)