{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Effect.Coroutine (
    Coroutine, Iterator (..), EffectCoroutine, runCoroutine, suspend
) where

import Control.Monad.Effect (Effect, Member, send, handle, eliminate, defaultRelay)

data Coroutine i o a = Coroutine (o -> a) i
  deriving Functor

data Iterator i o es a = Done a | Next (o -> Effect es (Iterator i o es a)) i

type EffectCoroutine i o es = (Member (Coroutine i o) es, '(i, o) ~ CoroutineType es)
type family CoroutineType es where
    CoroutineType (Coroutine i o ': es) = '(i, o)
    CoroutineType (e ': es) = CoroutineType es

suspend :: EffectCoroutine i o es => i -> Effect es o
suspend = send . Coroutine id

runCoroutine :: Effect (Coroutine i o ': es) a -> Effect es (Iterator i o es a)
runCoroutine =
    handle (return . Done)
    $ eliminate (\(Coroutine f k) -> return (Next f k))
    $ defaultRelay
