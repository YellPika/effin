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
    mask, unmask,

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

import Control.Applicative (Applicative (..))
import Control.Monad (join)

-- | An effectful computation. An @Effect l a@ may perform any of the effects
-- specified by the list of effects @l@ before returning a result of type @a@.
-- The definition is isomorphic to the following GADT:
--
-- @
-- data Effect l a where
--     Done :: a -> Effect l a
--     Side :: `Union` l (Effect l a) -> Effect l a
-- @
newtype Effect l a = Effect (forall r. (a -> r) -> (forall b. Union l b -> (b -> r) -> r) -> r)

unEffect :: (a -> r) -> (forall b. Union l b -> (b -> r) -> r) -> Effect l a -> r
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
send :: Member f l => f a -> Effect l a
send x = Effect $ \point bind -> bind (Union.inject x) point

-- | Executes an effect of type @f@ that produces a return value of type @r@.
-- Note that a specific instance of this function is of type
-- @Member f l => f (Effect l a) -> Effect l a@, which allows users
-- to send effects parameterized by effects.
sendEffect :: (Member f l, Effectful l r) => f r -> r
sendEffect x = relay (Union.inject x) id

-- | The class of types which result in an effect. That is:
--
-- > Effect l r
-- > a -> Effect l r
-- > a -> b -> Effect l r
-- > ...
class l ~ EffectsOf r => Effectful l r where
    -- | Determines the effects associated with the return type of a function.
    type family EffectsOf r :: Row (* -> *)

    relay :: Union l a -> (a -> r) -> r

    -- Prevents the `Minimal Complete Definition` box from showing.
    relay = undefined

instance Effectful l (Effect l a) where
    type EffectsOf (Effect l a) = l
    relay u f = join $ Effect $ \point bind -> bind u (point . f)

instance Effectful l r => Effectful l (a -> r) where
    type EffectsOf (a -> r) = EffectsOf r
    relay u f y = relay u (\x -> f x y)

-- | Handles an effect without eliminating it. The second function parameter is
-- passed an effect value and a continuation function.
--
-- The most common instantiation of this function is:
--
-- > (a -> Effect l b) -> (f (Effect l b) -> Effect l b) -> Effect l a -> Effect l b
intercept :: (Effectful l r, Member f l) => (a -> r) -> (forall b. f b -> (b -> r) -> r) -> Effect l a -> r
intercept point bind = unEffect point $ \u -> maybe (relay u) bind (Union.project u)

-- | Completely handles an effect. The second function parameter is passed an
-- effect value and a continuation function.
--
-- The most common instantiation of this function is:
--
-- > (a -> Effect l b) -> (f (Effect l b) -> Effect l b) -> Effect (f ': l) a -> Effect l b
eliminate :: Effectful l r => (a -> r) -> (forall b. f b -> (b -> r) -> r) -> Effect (f :+ l) a -> r
eliminate point bind = unEffect point (either bind relay . Union.pop)

-- | Adds an arbitrary effect to the head of the effect list.
extend :: Effect l a -> Effect (f :+ l) a
extend = translate Union.push

-- | Enables an effect that was previously disabled.
enable :: Effect (f :- l) a -> Effect l a
enable = translate Union.enable

-- | Hides an effect @f@ by translating each instance of the effect into an
-- equivalent effect further into the effect list.
--
-- prop> conceal = eliminate return (\x k -> send x >>= k)
conceal :: Member f l => Effect (f :+ l) a -> Effect l a
conceal = translate Union.conceal

-- | Hides an effect @f@ by translating each instance of the effect into an
-- equivalent effect at the head of the effect list.
reveal :: Member f l => Effect l a -> Effect (f :+ l) a
reveal = translate Union.reveal

-- | Translates the first effect in the effect list into another effect.
--
-- prop> rename f = eliminate return (\x k -> send (f x) >>= k) . swap . extend
rename :: (forall r. f r -> g r) -> Effect (f :+ l) a -> Effect (g :+ l) a
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

-- | Converts a set of effects @l@ into a single effect @f@.
--
-- @ mask f = `conceal` . `rename` f . `unflatten` @
mask :: (KnownLength l, Member f m) => (forall r. Union l r -> f r) -> Effect (l :++ m) a -> Effect m a
mask f = conceal . rename f . unflatten

-- | Converts an effect @f@ into a set of effects @l@.
--
-- @ unmask f = `flatten` . `rename` f . `reveal` @
unmask :: (Inclusive l, Member f m) => (forall r. f r -> Union l r) -> Effect m a -> Effect (l :++ m) a
unmask f = flatten . rename f . reveal

-- | A refined `Member`ship constraint that can infer @f@ from @l@, given
-- @name@. In order for this to be used, @`Is` name f@ must be defined.
-- For example:
--
-- > data Reader r a = ...
-- >
-- > type instance Is Reader f = IsReader f
-- >
-- > type IsReader f where
-- >     IsReader (Reader r) = True
-- >     IsReader f = False
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
