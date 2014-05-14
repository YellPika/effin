{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
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

import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Type.Equality ((:~:) (..), TestEquality (..))
import Control.Effect.Union
import Control.Effect.Witness
import Control.Monad.Effect

-- | Provides a base effect for exceptions. This effect allows the dynamic
-- generation of exception classes at runtime.
newtype Bracket s a = Bracket { unBracket :: Union '[Raise s, Witness s] a }
  deriving Functor

-- | The type of placeholder values indicating an exception class.
data Tag s a = Tag (a -> String) (Token s a)

instance TestEquality (Tag s) where
    testEquality (Tag _ i) (Tag _ j) = testEquality i j

class (Member (Bracket s) es, s ~ BracketType es) => EffectBracket s es
instance (Member (Bracket s) es, s ~ BracketType es) => EffectBracket s es

type family BracketType es where
    BracketType (Bracket s ': es) = s
    BracketType (e ': es) = BracketType es

-- | Creates a new tag. The function parameter describes the error message that
-- is shown in the case of an uncaught exception.
newTag :: EffectBracket s es => (a -> String) -> Effect es (Tag s a)
newTag toString = mask' $ Tag toString <$> newToken

-- | Raises an exception of the specified class and value.
raiseWith :: EffectBracket s es => Tag s b -> b -> Effect es a
raiseWith tag value = mask' $ send $ Raise tag value

-- | Specifies a handler for exceptions of a given class.
exceptWith :: EffectBracket s es => Tag s b -> Effect es a -> (b -> Effect es a) -> Effect es a
exceptWith tag effect handler = exceptAny effect [Handler tag handler]

-- | A handler for an exception. Use with `exceptAny`.
data Handler s es a where
    Handler :: Tag s b -> (b -> Effect es a) -> Handler s es a

-- | Specifies a number of handlers for exceptions thrown by the given
-- computation. This is prefered over chained calles to `exceptWith`, i.e.
--
-- > exceptWith t2 (exceptWith t1 m h1) h2
--
-- because @h2@ could catch exceptions thrown by @h1@.
exceptAny :: EffectBracket s es => Effect es a -> [Handler s es a] -> Effect es a
exceptAny effect handlers = effect `exceptAll` \i x ->
    let try (Handler j f) = (\Refl -> f x) <$> testEquality i j
        results = mapMaybe try handlers
    in fromMaybe (raiseWith i x) (listToMaybe results)

-- | Intercepts all exceptions. Used to implement `exceptWith` and `bracket`.
-- Not exported. Is it really a good thing to allow catching all exceptions?
-- The most common use case for catching all exceptions is to do cleanup, which
-- is what bracket is for.
exceptAll :: EffectBracket s es => Effect es a -> (forall b. Tag s b -> b -> Effect es a) -> Effect es a
exceptAll effect handler = mask' $ run $ unmask' effect
  where
    run =
        handle return
        $ intercept (\(Raise t x) -> unmask' (handler t x))
        $ defaultRelay

-- | Executes a computation with a resource, and ensures that the resource is
-- cleaned up afterwards.
bracket :: EffectBracket s es
        => Effect es a -- ^ The 'acquire' operation.
        -> (a -> Effect es ()) -- ^ The 'release' operation.
        -> (a -> Effect es b) -- ^ The computation to perform.
        -> Effect es b
bracket acquire destroy run = do
    resource <- acquire
    result <- run resource `exceptAll` \e x -> do
        destroy resource
        raiseWith e x
    destroy resource
    return result

-- | A specialized version of `bracket` which
-- does not require an 'acquire' operation.
finally :: EffectBracket s es => Effect es a -> Effect es () -> Effect es a
finally effect finalizer = bracket
    (return ())
    (\() -> finalizer)
    (\() -> effect)

-- | Executes a `Bracket` effect. The Rank-2 type ensures that `Tag`s do not
-- escape their scope.
runBracket :: (forall s. Effect (Bracket s ': es) a) -> Effect es a
runBracket effect = runWitness $ runRaise $ decompress $ translate unBracket effect
  where
    runRaise =
        handle return
        $ eliminate bind
        $ defaultRelay

    bind (Raise (Tag f _) x) = error (f x)

-- A couple helper functions for getting in and out of the base effects.
mask' :: EffectBracket s es => Effect (Raise s ': Witness s ': es) a -> Effect es a
mask' = mask Bracket . compress

unmask' :: EffectBracket s es => Effect es a -> Effect (Raise s ': Witness s ': es) a
unmask' = decompress . unmask unBracket

-- Quick and dirty exceptions (because Union works with existing functors).
data Raise s a where
    Raise :: Tag s b -> b -> Raise s a

instance Functor (Raise s) where
    fmap _ (Raise n x) = Raise n x
