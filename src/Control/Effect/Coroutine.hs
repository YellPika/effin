{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.Coroutine (
    EffectCoroutine, Coroutine, runCoroutine, suspend,
    Iterator (..), evalIterator
) where

import Control.Monad.Effect

-- | An effect describing a suspendable computation.
data Coroutine i o a = Coroutine (o -> a) i
  deriving Functor

class (Member (Coroutine i o) es, '(i, o) ~ CoroutineType es) => EffectCoroutine i o es
instance (Member (Coroutine i o) es, '(i, o) ~ CoroutineType es) => EffectCoroutine i o es

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

-- | A suspended computation.
data Iterator i o es a
    = Done a -- ^ Describes a finished computation.
    | Next (o -> Effect es (Iterator i o es a)) i
    -- ^ Describes a computation that provided a value
    -- of type `i` and awaits a value of type `o`.

-- | Evaluates an iterator by providing it with an input stream.
evalIterator :: Iterator i o es a -> [o] -> Effect es (Iterator i o es a, [i])
evalIterator (Next f v) (x:xs) = do
    i <- f x
    (r, vs) <- evalIterator i xs
    return (r, v:vs)
evalIterator i _ = return (i, [])
