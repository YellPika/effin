{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.Union (
    EffectUnion, Union, decompress, compress,
    KnownList, type (++)
) where

import Control.Monad.Effect
import Data.Union

class (KnownList es, Member (Union es) fs, es ~ UnionType fs) => EffectUnion es fs
instance (KnownList es, Member (Union es) fs, es ~ UnionType fs) => EffectUnion es fs

type family UnionType fs where
    UnionType (Union es ': fs) = es
    UnionType (f ': fs) = UnionType fs

-- | Distributes the sub-effects of a `Union` effect across a computation.
decompress :: KnownList es => Effect (Union es ': fs) a -> Effect (es ++ fs) a
decompress = transform flatten

-- | Collects some effects in a computation into a `Union` effect.
compress :: KnownList es => Effect (es ++ fs) a -> Effect (Union es ': fs) a
compress = transform unflatten

transform :: (forall r. Union es r -> Union fs r) -> Effect es a -> Effect fs a
transform f =
    handle return
    $ relay (withUnion sendEffect . f . inject)
