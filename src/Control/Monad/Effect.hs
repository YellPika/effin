{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.Effect (
    -- * The Effect Monad
    Effect, runEffect,
    send, sendEffect,

    -- * Effect Handlers
    Effectful (EffectsOf),

    eliminate, intercept,
    extend, enable,
    conceal, reveal, rename,
    swap, rotate,

    -- * Unions
    Union, flatten, unflatten,

    -- * Membership
    Member, MemberEffect, Is,

    -- * Effect Rows
    Row (..), type (:++),
    KnownLength, Inclusive
) where

import Data.Union (Union)
import qualified Data.Union as Union

import Data.Type.Row

import Control.Applicative (Applicative (..), (<$>))
import Control.Monad (join)

-- | An effectful computation. An @Effect es a@ may perform any of the effects
-- specified by the list of effects @l@ before returning a result of type @a@.
-- The definition is isomorphic to the following GADT:
--
-- @
-- data Effect es a where
--     Done :: a -> Effect es a
--     Side :: `Union` es (Effect es a) -> Effect es a
-- @
newtype Effect l a = Effect (forall r. (a -> r) -> (Union l r -> r) -> r)

unEffect :: (a -> r) -> (Union l r -> r) -> Effect l a -> r
unEffect point bind (Effect f) = f point bind

instance Functor (Effect l) where
    fmap f (Effect g) = Effect $ \point -> g (point . f)

instance Applicative (Effect l) where
    pure x = Effect $ \point _ -> point x
    Effect f <*> Effect x = Effect $ \point bind ->
        f (\f' -> x (point . f') bind) bind

instance Monad (Effect l) where
    return = pure
    Effect f >>= g = Effect $ \point bind ->
        f (unEffect point bind . g) bind

-- | Converts an computation that produces no effects into a regular value.
runEffect :: Effect Nil a -> a
runEffect (Effect f) = f id Union.absurd

-- | Executes an effect of type @f@ that produces a return value of type @a@.
send :: (Functor f, Member f l) => f a -> Effect l a
send x = Effect $ \point bind -> bind $ point <$> Union.inject x -- Inlined for efficiency (from relay).

-- | Executes an effect of type @f@ that produces a return value of type @r@.
-- Note that a specific instance of this function is of type
-- @(Functor f, Member f l) => f (Effect l a) -> Effect l a@, which allows users
-- to send effects parameterized by effects.
sendEffect :: (Functor f, Member f l, Effectful l r) => f r -> r
sendEffect = relay . Union.inject

-- | The class of types which result in an effect.
class l ~ EffectsOf r => Effectful l r where
    -- | Determines the effects associated with the return type of a function.
    type family EffectsOf r :: Row (* -> *)

    relay :: Union l r -> r

    -- Prevents the `Minimal Complete Definition` box from showing.
    relay = undefined

instance Effectful l (Effect l a) where
    type EffectsOf (Effect l a) = l
    relay u = join $ Effect $ \point bind -> bind $ point <$> u

instance Effectful l r => Effectful l (a -> r) where
    type EffectsOf (a -> r) = EffectsOf r
    relay u x = relay (fmap ($ x) u)

-- | Handles an effect without eliminating it. The given function is passed an
-- effect value parameterized by the output type (i.e. the return type of
-- `handle`).
intercept :: (Effectful l r, Member f l) => (a -> r) -> (f r -> r) -> Effect l a -> r
intercept point bind = unEffect point $ \u -> maybe (relay u) bind (Union.project u)

-- | Completely handles an effect. The given function is passed an effect value
-- parameterized by the output type (i.e. the return type of `handle`).
eliminate :: Effectful l r => (a -> r) -> (f r -> r) -> Effect (f :+ l) a -> r
eliminate point bind = unEffect point (either bind relay . Union.pop)

-- | Adds an arbitrary effect to the head of the effect list.
extend :: Effect l a -> Effect (f :+ l) a
extend = translate Union.push

-- | Enables an effect that was previously disabled.
enable :: Effect (f :- l) a -> Effect l a
enable = translate Union.enable

-- | Hides an effect @g@ by translating each instance of @g@ into an instance of
-- another effect @f@.
conceal :: Member f l => Effect (f :+ l) a -> Effect l a
conceal = translate Union.conceal

-- | Hides an effect @g@ by translating each instance of another effect @f@ into
-- an instance of @g@.
reveal :: Member f l => Effect l a -> Effect (f :+ l) a
reveal = translate Union.reveal

-- | Translates the first effect in the effect list into another effect.
rename :: Functor g => (forall r. f r -> g r) -> Effect (f :+ l) a -> Effect (g :+ l) a
rename f = translate (either (Union.inject . f) Union.push . Union.pop)

-- | Reorders the first two effects in a computation.
swap :: Effect (f :+ g :+ l) a -> Effect (g :+ f :+ l) a
swap = translate Union.swap

-- | Rotates the first three effects in a computation.
rotate :: Effect (f :+ g :+ h :+ l) a -> Effect (g :+ h :+ f :+ l) a
rotate = translate Union.rotate

-- | Distributes the sub-effects of a `Union` effect across a computation.
flatten :: Inclusive l => Effect (Union l :+ m) a -> Effect (l :++ m) a
flatten = translate Union.flatten

-- | Collects some effects in a computation into a `Union` effect.
unflatten :: KnownLength l => Effect (l :++ m) a -> Effect (Union l :+ m) a
unflatten = translate Union.unflatten

translate :: (forall r. Union l r -> Union m r) -> Effect l a -> Effect m a
translate f = unEffect return (relay . f)

-- | A refined `Member`ship constraint that can infer @f@ from @l@, given
-- @name@. In order for this to be used, @`Is` name f@ must be defined.
-- For example:
--
-- > data Reader r a = ...
-- >
-- > type instance Is Reader f where
-- >     Is Reader (Reader r) = True
-- >     Is Reader f = False
-- >
-- > type ReaderEffect r l = MemberEffect Reader (Reader r) l
-- >
-- > ask :: ReaderEffect r l => Effect l r
-- > ask = ...
--
-- Given the constraint @ReaderEffect r l@ in the above example, @r@ can be
-- inferred from @l@.
class (Member f l, f ~ InstanceOf name l) => MemberEffect name f l
instance (Member f l, f ~ InstanceOf name l) => MemberEffect name f l
