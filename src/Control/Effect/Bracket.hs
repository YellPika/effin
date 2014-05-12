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
    Exn, newExn, raiseWith, exceptWith, bracket,
    AnyExn
) where

import Control.Monad.Effect
import Unsafe.Coerce (unsafeCoerce)

data Exn s a = Exn (a -> String) Integer

castExn :: Exn s a -> Exn s b -> a -> Maybe b
castExn (Exn _ i) (Exn _ j) x | i == j = Just (unsafeCoerce x)
castExn _ _ _ = Nothing

data AnyExn where
    AnyExn :: Exn s a -> a -> AnyExn

instance Show AnyExn where
    show (AnyExn (Exn f _) x) = f x

data Bracket s a where
    Raise :: Exn s b -> b -> Bracket s a
    NewExn :: (b -> String) -> (Exn s b -> a) -> Bracket s a

instance Functor (Bracket s) where
    fmap _ (Raise e x) = Raise e x
    fmap f (NewExn toString g) = NewExn toString (f . g)

class (Member (Bracket s) es, s ~ BracketType es) => EffectBracket s es
instance (Member (Bracket s) es, s ~ BracketType es) => EffectBracket s es

type family BracketType es where
    BracketType (Bracket s ': es) = s
    BracketType (e ': es) = BracketType es

raiseWith :: EffectBracket s es => Exn s a -> a -> Effect es b
raiseWith e x = send (Raise e x)

exceptWith :: EffectBracket s es => Exn s b -> (b -> Effect es a) -> Effect es a -> Effect es a
exceptWith i f = exceptAll $ \j x ->
    maybe (raiseWith j x) f (castExn j i x)

exceptAll :: EffectBracket s es => (forall b. Exn s b -> b -> Effect es a) -> Effect es a -> Effect es a
exceptAll f =
    handle return
    $ intercept bind
    $ defaultRelay
  where
    bind (Raise e x) = f e x
    bind exn = sendEffect exn

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

newExn :: EffectBracket s es => (a -> String) -> Effect es (Exn s a)
newExn toString = send $ NewExn toString id

runBracket :: (forall s. Effect (Bracket s ': es) a) -> Effect es (Either AnyExn a)
runBracket effect =
    ( handle (\x _ -> return $ Right x)
    $ eliminate bind
    $ relay (\x s -> sendEffect $ fmap ($ s) x)
    ) effect (0 :: Integer)
  where
    bind (Raise e x) _ = return . Left $ AnyExn e x
    bind (NewExn toString f) s = f (Exn toString s) (s + 1)
