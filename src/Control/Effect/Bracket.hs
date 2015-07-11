{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.Bracket (
    EffectBracket, Bracket, runBracket,
    Tag, newTag, raiseWith, exceptWith,
    Handler, exceptAny, bracket, finally
) where

import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Type.Equality ((:~:) (..), TestEquality (..))
import Control.Effect.Witness
import Control.Monad.Effect

-- | Provides a base effect for exceptions. This effect allows the dynamic
-- generation of exception classes at runtime.
data Bracket s a where
    Raise :: Tag s b -> b -> Bracket s a
    BWitness :: Witness s a -> Bracket s a

-- | The type of placeholder values indicating an exception class.
data Tag s a = Tag (a -> String) (Token s a)

instance TestEquality (Tag s) where
    testEquality (Tag _ i) (Tag _ j) = testEquality i j

type instance Is Bracket f = IsBracket f

type family IsBracket f where
    IsBracket (Bracket s) = 'True
    IsBracket f = 'False

class MemberEffect Bracket (Bracket s) l => EffectBracket s l
instance MemberEffect Bracket (Bracket s) l => EffectBracket s l

-- | Creates a new tag. The function parameter describes the error message that
-- is shown in the case of an uncaught exception.
newTag :: EffectBracket s l => (a -> String) -> Effect l (Tag s a)
newTag toString = conceal $ fmap (Tag toString) (rename BWitness newToken)

-- | Raises an exception of the specified class and value.
raiseWith :: EffectBracket s l => Tag s b -> b -> Effect l a
raiseWith tag value = send $ Raise tag value

-- | Specifies a handler for exceptions of a given class.
exceptWith :: EffectBracket s l => Tag s b -> Effect l a -> (b -> Effect l a) -> Effect l a
exceptWith tag effect handler = exceptAny effect [Handler tag handler]

-- | A handler for an exception. Use with `exceptAny`.
data Handler s l a where
    Handler :: Tag s b -> (b -> Effect l a) -> Handler s l a

-- | Specifies a number of handlers for exceptions thrown by the given
-- computation. This is prefered over chained calles to `exceptWith`, i.e.
--
-- > exceptWith t2 (exceptWith t1 m h1) h2
--
-- because @h2@ could catch exceptions thrown by @h1@.
exceptAny :: EffectBracket s l => Effect l a -> [Handler s l a] -> Effect l a
exceptAny effect handlers = effect `exceptAll` \i x ->
    let try (Handler j f) = fmap (\Refl -> f x) (testEquality i j)
        results = mapMaybe try handlers
    in fromMaybe (raiseWith i x) (listToMaybe results)

-- | Intercepts all exceptions. Used to implement `exceptWith` and `bracket`.
-- Not exported. Is it really a good thing to allow catching all exceptions?
-- The most common use case for catching all exceptions is to do cleanup, which
-- is what bracket is for.
exceptAll :: EffectBracket s l => Effect l a -> (forall b. Tag s b -> b -> Effect l a) -> Effect l a
exceptAll effect handler = intercept
    return
    (\b k ->
        case b of
            Raise t x -> handler t x
            _ -> send b >>= k)
    effect

-- | Executes a computation with a resource, and ensures that the resource is
-- cleaned up afterwards.
bracket :: EffectBracket s l
        => Effect l a -- ^ The 'acquire' operation.
        -> (a -> Effect l ()) -- ^ The 'release' operation.
        -> (a -> Effect l b) -- ^ The computation to perform.
        -> Effect l b
bracket acquire destroy run = do
    resource <- acquire
    result <- run resource `exceptAll` \e x -> do
        destroy resource
        raiseWith e x
    destroy resource
    return result

-- | A specialized version of `bracket` which
-- does not require an 'acquire' operation.
finally :: EffectBracket s l => Effect l a -> Effect l () -> Effect l a
finally effect finalizer = bracket
    (return ())
    (const finalizer)
    (const effect)

-- | Executes a `Bracket` effect. The Rank-2 type ensures that `Tag`s do not
-- escape their scope.
runBracket :: (forall s. Effect (Bracket s ':+ l) a) -> Effect l a
runBracket effect = runWitness (convert effect)

convert :: Effect (Bracket s ':+ l) a -> Effect (Witness s ':+ l) a
convert =
    eliminate
        return
        (\t k ->
            case t of
                Raise (Tag f _) x -> error (f x)
                BWitness w -> send w >>= k)
    . swap
    . extend
