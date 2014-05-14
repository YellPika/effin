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
    raiseWith, exceptWith, bracket
) where

import Data.Type.Equality ((:~:) (..), TestEquality (..))
import Control.Effect.Union
import Control.Effect.Witness
import Control.Monad.Effect

newtype Bracket s a = Bracket { unBracket :: Union '[Raise s, Witness s] a }

class (Member (Bracket s) es, s ~ BracketType es) => EffectBracket s es
instance (Member (Bracket s) es, s ~ BracketType es) => EffectBracket s es

type family BracketType es where
    BracketType (Bracket s ': es) = s
    BracketType (e ': es) = BracketType es

raiseWith :: EffectBracket s es => Token s b -> b -> Effect es a
raiseWith t x = mask' $ send $ Raise t x

exceptWith :: EffectBracket s es => Token s b -> (b -> Effect es a) -> Effect es a -> Effect es a
exceptWith i f = exceptAll $ \j x ->
    maybe (raiseWith j x) (\Refl -> f x) (testEquality i j)

exceptAll :: EffectBracket s es => (forall b. Token s b -> b -> Effect es a) -> Effect es a -> Effect es a
exceptAll f = mask' . exceptAll' (\t v -> unmask' (f t v)) . unmask'

bracket :: EffectBracket s es => Effect es a -> (a -> Effect es ()) -> (a -> Effect es b) -> Effect es b
bracket acquire destroy run = do
    resource <- acquire
    result <- exceptAll
        (\e x -> do
            destroy resource
            raiseWith e x)
        (run resource)
    destroy resource
    return result

runBracket :: (forall s. Effect (Bracket s ': es) a) -> Effect es (Either AnyToken a)
runBracket effect = runWitness $ runRaise $ decompress $ translate unBracket effect

mask' :: EffectBracket s es => Effect (Raise s ': Witness s ': es) a -> Effect es a
mask' = mask Bracket . compress

unmask' :: EffectBracket s es => Effect es a -> Effect (Raise s ': Witness s ': es) a
unmask' = decompress . unmask unBracket

data AnyToken where
    AnyToken :: Token s b -> b -> AnyToken

data Raise s a where
    Raise :: Token s b -> b -> Raise s a

instance Functor (Raise s) where
    fmap _ (Raise n x) = Raise n x

exceptAll' :: Member (Raise s) es => (forall b. Token s b -> b -> Effect es a) -> Effect es a -> Effect es a
exceptAll' handler =
    handle return
    $ intercept (\(Raise t x) -> handler t x)
    $ defaultRelay

runRaise :: Effect (Raise s ': es) a -> Effect es (Either AnyToken a)
runRaise =
    handle (return . Right)
    $ eliminate (\(Raise n x) -> return $ Left $ AnyToken n x)
    $ defaultRelay
