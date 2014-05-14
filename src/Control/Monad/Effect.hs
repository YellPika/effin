{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | This module provides four things:
--
-- 1. An `Effect` monad for representing effectful computations,
-- 2. A DSL for effect handling that lets you cleanly handle an arbitrary number of effects,
-- 3. Combinators for encapsulating effects, and
-- 4. A type-level list membership constraint.
module Control.Monad.Effect (
    -- * The Effect Monad
    Effect, runEffect,
    send, sendEffect,

    -- * Effect Handlers
    -- | The following types and functions form a small DSL that allows users to
    -- specify how to handle effects. A handler can be formed by a call to
    -- `handle`, followed by a chain of calls to `eliminate` and `intercept`,
    -- and ended by either a `defaultRelay`, `emptyRelay`, or a call to `relay`.
    --
    -- For example, a possible handler for the state effect would be:
    --
    -- > data State s a = State (s -> (s, a))
    -- >
    -- > runState :: Effect (State s ': es) a -> s -> Effect es (s, a)
    -- > runState =
    -- >     handle (\output state -> return (state, output))
    -- >     $ eliminate (\(State transform) state ->
    -- >         let (state', continue) = transform state
    -- >         in continue state')
    -- >     $ relay (\effect state -> do
    -- >         continue <- sendEffect effect
    -- >         return (continue state))
    --
    -- As an analogy to monads, `handle` lets you specify the return function,
    -- while `eliminate`, `intercept`, and `relay`, let you specify the bind
    -- function.
    Handler, handle,
    eliminate, intercept,
    relay, defaultRelay, emptyRelay,

    -- * Effect Encapsulation
    mask, unmask, translate,

    -- * Membership
    Member
) where

import Data.Union
import Control.Applicative (Applicative (..), (<$>))
import Control.Monad (join)

-- | An effectful computation. An @Effect es a@ may perform any of the effects
-- specified by the list of effects @es@ before returning a result of type @a@.
-- The definition is isomorphic to the following GADT:
--
-- > data Effect es a where
-- >     Done :: a -> Effect es a
-- >     Side :: `Union` es (Effect es a) -> Effect es a
newtype Effect es a = Effect {
    unEffect :: forall r. (a -> r) -> Handler es r -> r
} deriving Functor

instance Applicative (Effect es) where
    pure x = Effect $ \p _ -> p x
    Effect f <*> Effect x = Effect $ \p b ->
        f (\f' -> x (p . f') b) b

instance Monad (Effect es) where
    return = pure
    Effect f >>= g = Effect $ \p b ->
        f (\x -> unEffect (g x) p b) b

-- | Converts an computation that produces no effects into a regular value.
runEffect :: Effect '[] a -> a
runEffect (Effect f) = f id undefined

-- | Executes an effect of type @e@ that produces a return value of type @a@.
send :: (Functor e, Member e es) => e a -> Effect es a
send x = Effect $ \point (Handler bind) -> bind $ inject $ point <$> x

-- | Executes an effect of type @e@ that produces a return value of type @a@.
sendEffect :: (Functor e, Member e es) => e (Effect es a) -> Effect es a
sendEffect = join . send

-- | A handler for an effectful computation.
-- Combined with 'handle', allows one to convert a computation
-- parameterized by the effect list @es@ to a value of type @a@.
newtype Handler es r = Handler (Union es r -> r)

-- | @handle p h@ transforms an effect into a value of type @b@.
--
-- @p@ specifies how to convert pure values. That is,
--
-- prop> handle p h (return x) = p x
--
-- @h@ specifies how to handle effects.
handle :: (a -> r) -> Handler es r -> Effect es a -> r
handle point bind (Effect f) = f point bind

-- | Provides a way to completely handle an effect. The given function is passed
-- an effect value parameterized by the output type (i.e. the return type of
-- `handle`).
eliminate :: (e r -> r) -> Handler es r -> Handler (e ': es) r
eliminate bind (Handler pass) = Handler (either pass bind . reduce)

-- | Provides a way to handle an effect without eliminating it. The given
-- function is passed an effect value parameterized by the output type (i.e. the
-- return type of `handle`).
intercept :: Member e es => (e r -> r) -> Handler es r -> Handler es r
intercept bind (Handler pass) = Handler $ \u ->
    maybe (pass u) bind (project u)

-- | Provides a way to add arbitrary effects to the head of the effect list.
-- Not exported because no effect seems to need it.
ignore :: Functor e => Handler (e ': es) r -> Handler es r
ignore (Handler pass) = Handler (pass . extend . Left)

-- | Computes a basis handler. Provides a way to pass on effects of unknown
-- types. In most cases, `defaultRelay` is sufficient.
relay :: (forall e. Member e es => e b -> b) -> Handler es b
relay f = Handler (withUnion f)

-- | Relays all effects without examining them.
--
-- prop> handle id defaultRelay x = x
defaultRelay :: Handler es (Effect es a)
defaultRelay = relay sendEffect

-- | A handler for when there are no effects. Since `Handler`s handle effects,
-- they cannot be run on a computation that never produces an effect. By the
-- principle of explosion, a handler that requires exactly zero effects can
-- produce any value.
emptyRelay :: Handler '[] a
emptyRelay = Handler absurdUnion

-- | Hides an effect. All effects of type @e@ are translated
-- to effects of type @f@, and @e@ is erased from the effect list.
mask :: Member f es => (forall r. e r -> f r) -> Effect (e ': es) a -> Effect es a
mask f =
    handle return
    $ eliminate (sendEffect . f)
    $ defaultRelay

-- | Reveals an effect. All effects of type @f@ are translated
-- to effects of type @e@, and @e@ is added to the effect list.
unmask :: (Functor e, Member f es) => (forall r. f r -> e r) -> Effect es a -> Effect (e ': es) a
unmask f =
    handle return
    $ intercept (sendEffect . f)
    $ ignore
    $ defaultRelay

-- | Translates an effect of one type to another.
translate :: Functor f => (forall r. e r -> f r) -> Effect (e ': es) a -> Effect (f ': es) a
translate f =
    handle return
    $ eliminate (sendEffect . f)
    $ ignore
    $ defaultRelay
