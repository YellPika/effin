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
    Tag, newTag, raiseWith, exceptWith, bracket, finally
) where

import Data.Type.Equality ((:~:) (..), TestEquality (..))
import Data.Union
import Control.Effect.Union
import Control.Effect.Witness
import Control.Monad.Effect

-- | Provides a base effect for exceptions. This effect allows the dynamic
-- generation of exception classes at runtime.
newtype Bracket s a = Bracket { unBracket :: Union '[Raise s, Witness s] a }
  deriving Functor

class (Member (Bracket s) es, s ~ BracketType es) => EffectBracket s es
instance (Member (Bracket s) es, s ~ BracketType es) => EffectBracket s es

-- | The type of placeholder values indicating an exception class.
data Tag s a = Tag (a -> String) (Token s a)

type family BracketType es where
    BracketType (Bracket s ': es) = s
    BracketType (e ': es) = BracketType es

-- | Creates a new tag. The function parameter describes the error message that
-- is shown in the case of an uncaught exception.
newTag :: EffectBracket s es => (a -> String) -> Effect es (Tag s a)
newTag toString = fmap (Tag toString) $ mask' $ newToken "Bracket"

-- | Raises an exception of the specified class and value.
raiseWith :: EffectBracket s es => Tag s b -> b -> Effect es a
raiseWith t x = mask' $ send $ Raise t x

-- | Specifies a handler for exceptions of a given class.
exceptWith :: EffectBracket s es => Tag s b -> (b -> Effect es a) -> Effect es a -> Effect es a
exceptWith (Tag _ i) f = exceptAll $ \t@(Tag _ j) x ->
    maybe (raiseWith t x) (\Refl -> f x) (testEquality i j)

-- | Intercepts all exceptions. Used to implement `exceptWith` and `bracket`.
-- Not exported. Is it really a good thing to allow catching all exceptions?
-- The most common use case for catching all exceptions is to do cleanup, which
-- is what bracket is for.
exceptAll :: EffectBracket s es => (forall b. Tag s b -> b -> Effect es a) -> Effect es a -> Effect es a
exceptAll handler = mask' . run . unmask'
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
    result <- exceptAll
        (\e x -> do
            destroy resource
            raiseWith e x)
        (run resource)
    destroy resource
    return result

-- | A specialized version of `bracket` which
-- does not require an 'acuiqre' operation.
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
        $ eliminate (\(Raise (Tag f _) x) -> error (f x))
        $ defaultRelay

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
