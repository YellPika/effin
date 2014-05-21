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

type instance Is Coroutine f = IsCoroutine f

type family IsCoroutine f where
    IsCoroutine (Coroutine i o) = True
    IsCoroutine f = False

class MemberEffect Coroutine (Coroutine i o) es => EffectCoroutine i o es
instance MemberEffect Coroutine (Coroutine i o) es => EffectCoroutine i o es

-- | Suspends the current computation by providing a value
-- of type `i` and then waiting for a value of type `o`.
suspend :: EffectCoroutine i o es => i -> Effect es o
suspend = send . Coroutine id

-- | Converts a `Coroutine` effect into an `Iterator`.
runCoroutine :: Effect (Coroutine i o :+ es) a -> Effect es (Iterator i o es a)
runCoroutine = eliminate (return . Done) (\(Coroutine f k) -> return (Next f k))

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
