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
    Tag, newTag, raiseWith, exceptWith, bracket,
    AnyTag
) where

import Data.Type.Equality ((:~:) (..), TestEquality (..))
import Data.Union
import Control.Effect.Union
import Control.Effect.Witness
import Control.Monad.Effect

newtype Bracket s a = Bracket { unBracket :: Union '[Raise s, Witness s] a }
  deriving Functor

class (Member (Bracket s) es, s ~ BracketType es) => EffectBracket s es
instance (Member (Bracket s) es, s ~ BracketType es) => EffectBracket s es

newtype Tag s a = Tag (Token s a)

type family BracketType es where
    BracketType (Bracket s ': es) = s
    BracketType (e ': es) = BracketType es

newTag :: EffectBracket s es => String -> Effect es (Tag s a)
newTag = fmap Tag . mask' . newToken

raiseWith :: EffectBracket s es => Tag s b -> b -> Effect es a
raiseWith t x = mask' $ send $ Raise t x

exceptWith :: EffectBracket s es => Tag s b -> (b -> Effect es a) -> Effect es a -> Effect es a
exceptWith (Tag i) f = exceptAll $ \t@(Tag j) x ->
    maybe (raiseWith t x) (\Refl -> f x) (testEquality i j)

exceptAll :: EffectBracket s es => (forall b. Tag s b -> b -> Effect es a) -> Effect es a -> Effect es a
exceptAll handler = mask' . run . unmask'
  where
    run =
        handle return
        $ intercept (\(Raise t x) -> unmask' (handler t x))
        $ defaultRelay

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

runBracket :: (forall s. Effect (Bracket s ': es) a) -> Effect es (Either AnyTag a)
runBracket effect = runWitness $ runRaise $ decompress $ translate unBracket effect
  where
    runRaise =
        handle (return . Right)
        $ eliminate (\(Raise n x) -> return $ Left $ AnyTag n x)
        $ defaultRelay

mask' :: EffectBracket s es => Effect (Raise s ': Witness s ': es) a -> Effect es a
mask' = mask Bracket . compress

unmask' :: EffectBracket s es => Effect es a -> Effect (Raise s ': Witness s ': es) a
unmask' = decompress . unmask unBracket

data AnyTag where
    AnyTag :: Tag s b -> b -> AnyTag

data Raise s a where
    Raise :: Tag s b -> b -> Raise s a

instance Functor (Raise s) where
    fmap _ (Raise n x) = Raise n x
