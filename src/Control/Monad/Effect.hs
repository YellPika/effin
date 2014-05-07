{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | This module provides three things:
--
-- 1. An `Effect` monad for representing effectful computations,
-- 2. A DSL for effect handling that lets you cleanly handle an arbitrary number of effects, and
-- 3. A type-level list membership constraint.
module Control.Monad.Effect (
    -- * The Effect Monad
    Effect,
    runEffect, send,

    -- * Effect Handlers
    -- | The following types and functions form a small DSL that allows users to
    -- specify how to handle effects. A handler can be formed by a call to
    -- `handle`, followed by a chain of calls to `eliminate` and `intercept`,
    -- and ending in either a `defaultRelay`, `emptyRelay`, or a call to
    -- `relay`.
    --
    -- For example, the following is a handler for the state effect.
    --
    -- > data State s a = State (s -> (s, a))
    -- >
    -- > runState :: Effect (State s ': es) a -> s -> Effect es (s, a)
    -- > runState =
    -- >     handle (\output state -> return (state, output))
    -- >     $ eliminate (\continue (State transform) state ->
    -- >         let (state', output) = transform state
    -- >         in continue output state')
    -- >     $ relay (\continue effect state -> do
    -- >         output <- send effect
    -- >         continue output state)
    --
    -- As an analogy to monads, `handle` lets you specify the return function,
    -- while `eliminate`, `intercept`, and `relay` let you specify the bind
    -- function.
    Handler, handle,
    eliminate, intercept,
    relay, defaultRelay, emptyRelay,

    -- * Membership
    Member
) where

import Control.Applicative (Applicative (..))
import Control.Monad (ap, liftM)
import Data.Union (Union, Member, inject, project, reduce, withUnion, absurdUnion)

-- | An effectful computation. An @Effect es a@ may perform any of the effects
-- specified by the list of effects @es@ before returning a result of type @a@.
data Effect es a = Done a | Side (Union es (Effect es a))

instance Functor (Effect es) where
    fmap = liftM

instance Applicative (Effect es) where
    pure = return
    (<*>) = ap

instance Monad (Effect es) where
    return = Done
    Done x >>= f = f x
    Side x >>= f = Side (fmap (>>= f) x)

-- | Converts an computation that produces no effects into a regular value.
runEffect :: Effect '[] a -> a
runEffect (Done x) = x
runEffect (Side u) = absurdUnion u

-- | Executes an effect of type @e@ that produces a return value of type @a@.
send :: Member e es => e (Effect es a) -> Effect es a
send = Side . inject

-- | A handler for an effectful computation.
-- Combined with 'handle', allows one to convert a computation
-- parameterized by the effect list @es@ to a value of type @a@.
data Handler es a = Handler (Union es a -> a)

-- | @handle p h@ transforms an effect into a value of type @b@.
--
-- @p@ specifies how to convert pure values. That is,
--
-- prop> handle p h (return x) = p x
--
-- @h@ specifies how to handle effects.
handle :: (a -> b) -> Handler es b -> Effect es a -> b
handle point (Handler bind) = run
  where
    run (Done x) = point x
    run (Side x) = bind (fmap run x)

-- | Provides a way to completely handle an effect.
-- The given function is passed a continuation and an effect value.
-- The universally quantified variable @c@ can be thought of as the type of
-- value that the user code is expecting to receive from a call to `send`.
eliminate :: (e b -> b) -> Handler es b -> Handler (e ': es) b
eliminate bind (Handler pass) = Handler (either pass bind . reduce)

-- | Provides a way to handle an effect without eliminating it.
-- The given function is passed a continuation and an effect value.
intercept :: Member e es => (e b -> b) -> Handler es b -> Handler es b
intercept bind (Handler pass) = Handler $ \u -> maybe (pass u) bind (project u)

-- | Computes a basis handler. Provides a way to pass on effects of unknown
-- types. In most cases, `defaultRelay` is sufficient.
relay :: (forall e. Member e es => e b -> b) -> Handler es b
relay f = Handler (withUnion f)

-- | Relays all effects without examining them.
--
-- prop> handle id defaultRelay x = x
defaultRelay :: Handler es (Effect es a)
defaultRelay = relay send

-- | A handler for when there are no effects. Since `Handler`s handle effects,
-- they cannot be run on a computation that never produces an effect. By the
-- principle of explosion, a handler that requires exactly zero effects can
-- produce any value.
emptyRelay :: Handler '[] a
emptyRelay = Handler absurdUnion
