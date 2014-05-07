{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}

module Control.Effect.Monad (
    EffectMonad, runMonad, lift
) where

import Control.Monad (join)
import Control.Monad.Effect (Effect, Member, send, handle, eliminate, emptyRelay)

type EffectMonad m es = (Member m es, Monad m)

runMonad :: Monad m => Effect '[m] a -> m a
runMonad =
    handle return
    $ eliminate join
    $ emptyRelay

lift :: EffectMonad m es => m a -> Effect es a
lift = send . fmap return
