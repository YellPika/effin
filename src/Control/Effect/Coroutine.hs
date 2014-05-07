{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Effect.Coroutine (
    Coroutine, Iterator (..), EffectCoroutine, runCoroutine, yield
) where

import Control.Monad.Effect (Effect, Member, send, handle, eliminate, defaultRelay)

data Coroutine i o a = Coroutine (o -> a) i
data Iterator i o es a = Done a | Next (o -> Effect es (Iterator i o es a)) i

type EffectCoroutine i o es = (Member (Coroutine i o) es, '(i, o) ~ CoroutineType es)
type family CoroutineType es where
    CoroutineType (Coroutine i o ': es) = '(i, o)
    CoroutineType (e ': es) = CoroutineType es

yield :: EffectCoroutine i o es => i -> Effect es o
yield = send . Coroutine id

runCoroutine :: Effect (Coroutine i o ': es) a -> Effect es (Iterator i o es a)
runCoroutine =
    handle (return . Done)
    $ eliminate (\k (Coroutine f x) -> return $ Next (k . f) x)
    $ defaultRelay
