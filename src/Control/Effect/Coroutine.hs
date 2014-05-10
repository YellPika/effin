{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Effect.Coroutine (
    EffectCoroutine, Coroutine, Iterator (..), runCoroutine, suspend
) where

import Control.Monad.Effect

-- | An effect describing a suspendable computation.
data Coroutine i o a = Coroutine (o -> a) i
  deriving Functor

-- | A suspended computation.
data Iterator i o es a
    = Done a -- ^ Describes a finished computation.
    | Next (o -> Effect es (Iterator i o es a)) i
    -- ^ Describes a computation that provided a value
    -- of type `i` and awaits a value of type `o`.

type EffectCoroutine i o es = (Member (Coroutine i o) es, '(i, o) ~ CoroutineType es)
type family CoroutineType es where
    CoroutineType (Coroutine i o ': es) = '(i, o)
    CoroutineType (e ': es) = CoroutineType es

-- | Suspends the current computation by providing a value
-- of type `i` and then waiting for a value of type `o`.
suspend :: EffectCoroutine i o es => i -> Effect es o
suspend = send . Coroutine id

-- | Converts a `Coroutine` effect into an `Iterator`.
runCoroutine :: Effect (Coroutine i o ': es) a -> Effect es (Iterator i o es a)
runCoroutine =
    handle (return . Done)
    $ eliminate (\(Coroutine f k) -> return (Next f k))
    $ defaultRelay
