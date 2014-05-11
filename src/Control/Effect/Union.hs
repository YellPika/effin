{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.Union (
    EffectUnion, Union, runUnion, nest,
    KnownList, type (++),
) where

import Control.Monad.Effect
import Data.Union

class (KnownList es, Member (Union es) fs, es ~ UnionType fs) => EffectUnion es fs
instance (KnownList es, Member (Union es) fs, es ~ UnionType fs) => EffectUnion es fs

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
