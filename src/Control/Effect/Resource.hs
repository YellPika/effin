{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Effect.Resource where

import Control.Applicative ((<$>))
import Control.Arrow (second)
import Control.Effect.Lift (EffectLift, lift)
import Control.Monad.Effect (Effect, Member, send, handle, eliminate, defaultRelay)

data Resource (m :: * -> *) a = Resource a (m ())
  deriving Functor

type EffectResource m es = (Member (Resource m) es, m ~ ResourceType es)
type family ResourceType es where
    ResourceType (Resource m ': es) = m
    ResourceType (e ': es) = ResourceType es

register :: EffectResource m es => m () -> Effect es ()
register = send . Resource (return ())

runResource :: EffectLift m es => Effect (Resource m ': es) a -> Effect es a
runResource effect = do
    (output, destructors) <- run effect
    lift (sequence_ destructors)
    return output
  where
    run =
        handle (\x -> return (x, []))
        $ eliminate (\(Resource k d) -> second (d:) <$> k)
        $ defaultRelay
