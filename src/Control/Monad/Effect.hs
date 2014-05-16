{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.Effect (
    -- * The Effect Monad
    Effect, runEffect,
    send, sendEffect,

    -- * Effect Handlers
    EffectHandler, ($:),
    handle, handleBy,
    eliminate, intercept,
    extend, reveal,
    compress, decompress,
    mask, unmask, rename,

    Effectful, EffectsOf,

    -- * Membership
    Member, MemberEffect, Is
) where

import Data.Type.List
import Data.Union
import Control.Applicative (Applicative (..), (<$>))
import Control.Category (Category (..), (>>>))
import Control.Monad (join)
import Prelude hiding (id, (.))

-- | An effectful computation. An @Effect es a@ may perform any of the effects
-- specified by the list of effects @es@ before returning a result of type @a@.
-- The definition is isomorphic to the following GADT:
--
-- @
-- data Effect es a where
--     Done :: a -> Effect es a
--     Side :: `Union` es (Effect es a) -> Effect es a
-- @
newtype Effect l a = Effect {
    unEffect :: forall r. (a -> r) -> (Union l r -> r) -> r
}

instance Functor (Effect l) where
    fmap f (Effect g) = Effect $ \point -> g (point . f)

instance Applicative (Effect l) where
    pure x = Effect $ \point _ -> point x
    Effect f <*> Effect x = Effect $ \point bind ->
        f (\f' -> x (point . f') bind) bind

instance Monad (Effect l) where
    return = pure
    Effect f >>= g = Effect $ \point bind ->
        f (\x -> unEffect (g x) point bind) bind

-- | Converts an computation that produces no effects into a regular value.
runEffect :: Effect Nil a -> a
runEffect (Effect f) = f id absurdUnion

-- | The class of types which result in an effect.
class l ~ EffectsOf r => Effectful l r where
    -- | Determines the effects associated with the return type of a function.
    type family EffectsOf r :: List (* -> *)

    relay :: Union l r -> r

    -- Prevents the `Minimal Complete Definition` box from showing.
    relay = undefined

instance Effectful l (Effect l a) where
    type EffectsOf (Effect l a) = l
    relay = join . sendUnion

instance Effectful l r => Effectful l (a -> r) where
    type EffectsOf (a -> r) = EffectsOf r
    relay u x = relay (fmap ($ x) u)

-- | Executes an effect of type @f@ that produces a return value of type @r@.
-- Note that a specific instance of this function is of type
-- @(Functor f, Member f l) => f (Effect l a) -> Effect l a@, which allows users
-- to send effects parameterized by effects.
sendEffect :: (Functor f, Member f l, Effectful l r) => f r -> r
sendEffect = relay . inject

-- | Executes an effect of type @f@ that produces a return value of type @a@.
send :: (Functor f, Member f l) => f a -> Effect l a
send = sendUnion . inject

sendUnion :: Union l a -> Effect l a
sendUnion x = Effect $ \point bind -> bind $ point <$> x

-- | A handler for an effectful computation. Combined with 'handle', allows one
-- to convert a computation parameterized by the effect list @l@ to a value of
-- type @r@ that is parameterized by the effect list @m@.
newtype EffectHandler r l m = EffectHandler ((Union m r -> r) -> Union l r -> r)

infixr 1 $:

-- | `EffectHandler` composition. Equivalent to @`flip` (`.`)@, where `.` is
-- from @Control.Category@.
($:) :: EffectHandler r l m -> EffectHandler r m n -> EffectHandler r l n
($:) = (>>>)

instance Category (EffectHandler r) where
    id = EffectHandler id
    EffectHandler f . EffectHandler g = EffectHandler (g . f)

-- | @handle p h@ transforms an effect into a value of type @r@.
--
-- @p@ specifies how to convert pure values. That is,
--
-- prop> handle p h (return x) = p x
--
-- @h@ specifies how to handle effects.
-- The return type @r@ must be `Effectful`, i.e. it must either be
-- an `Effect` parameterized by @m@, or a function that returns an
-- `Effect` parameterized by @m@.
handle :: Effectful m r => (a -> r) -> EffectHandler r l m -> Effect l a -> r
handle point (EffectHandler bind) (Effect f) = f point (bind relay)

-- | A special case of `handle` where the return value is parameterized by the
-- same result type as the input effect.
--
-- prop> handleBy = handle return
handleBy :: EffectHandler (Effect m a) l m -> Effect l a -> Effect m a
handleBy = handle return

-- | Completely handles an effect. The given function is passed an effect value
-- parameterized by the output type (i.e. the return type of `handle`).
eliminate :: (f r -> r) -> EffectHandler r (f :+ l) l
eliminate bind = EffectHandler $ \pass -> either bind pass . reduce

-- | Handles an effect without eliminating it. The given function is passed an
-- effect value parameterized by the output type (i.e. the return type of
-- `handle`).
intercept :: Member f l => (f r -> r) -> EffectHandler r l l
intercept bind = EffectHandler $ \pass u -> maybe (pass u) bind (project u)

-- | Adds an arbitrary effect to the head of the effect list.
extend :: Functor f => EffectHandler r l (f :+ l)
extend = translate (expand . Right)

-- | Unhides an effect.
reveal :: (Functor f, Member f l) => EffectHandler r (f :- l) l
reveal = translate (insert . Right)

-- | Hides an effect @g@ by translating each instance of @g@ into an instance of
-- another effect @f@.
mask :: (Functor f, Member f l) => (forall a. g a -> f a) -> EffectHandler r (g :+ l) l
mask f = translate (either (inject . f) id . reduce)

-- | Hides an effect @g@ by translating each instance of another effect @f@ into
-- an instance of @g@.
unmask :: (Functor g, Member f l) => (forall a. f a -> g a) -> EffectHandler r l (g :+ l)
unmask f = translate $ \u -> maybe (expand (Right u)) (inject . f) (project u)

-- | Translates the first effect in the effect list into another effect.
rename :: Functor g => (forall a. f a -> g a) -> EffectHandler r (f :+ l) (g :+ l)
rename f = EffectHandler $ \pass -> pass . either (inject . f) (expand . Right) . reduce

-- | Distributes the sub-effects of a `Union` effect across a computation.
compress :: Inclusive l => EffectHandler r (Union l :+ m) (l ++ m)
compress = translate flatten

-- | Collects some effects in a computation into a `Union` effect.
decompress :: KnownList l => EffectHandler r (l ++ m) (Union l :+ m)
decompress = translate unflatten

translate :: (Union l r -> Union m r) -> EffectHandler r l m
translate f = EffectHandler $ \pass -> pass . f

-- | A refined `Member`ship constraint that can infer @f@ from @l@, given
-- @name@. In order for this to be used, @`Is` name f@ must be defined.
-- For example:
--
-- > data Reader r a = ...
-- > data ReaderType
-- >
-- > type EffectReader r l = MemberEffect ReaderType (Reader r) l
-- >
-- > ask :: EffectReader r l => Effect l r
-- > ask = ...
class (Member f l, f ~ InstanceOf name l) => MemberEffect name f l
instance (Member f l, f ~ InstanceOf name l) => MemberEffect name f l
