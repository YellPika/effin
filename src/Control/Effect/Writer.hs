{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Effect.Writer (
    EffectWriter, Writer, runWriter,
    tell, listen, listens, pass, censor
) where

import Control.Applicative ((<$>))
import Control.Arrow (second)
import Control.Monad.Effect (Effect, Member, send, handle, eliminate, intercept, defaultRelay)
import Data.Monoid (Monoid (..))

data Writer w a = Writer w a
  deriving Functor

type EffectWriter w es = (Monoid w, Member (Writer w) es, w ~ WriterType es)
type family WriterType es where
    WriterType (Writer w ': es) = w
    WriterType (t ': es) = WriterType es

tell :: EffectWriter w es => w -> Effect es ()
tell x = send $ Writer x $ return ()

listen :: EffectWriter w es => Effect es a -> Effect es (a, w)
listen =
    handle point
    $ intercept bind
    $ defaultRelay

listens :: EffectWriter w es => (w -> b) -> Effect es a -> Effect es (a, b)
listens f = fmap (second f) . listen

pass :: EffectWriter w es => Effect es (a, w -> w) -> Effect es a
pass effect = do
    ((x, f), l) <- listen effect
    tell (f l)
    return x

censor :: EffectWriter w es => (w -> w) -> Effect es a -> Effect es a
censor f =
    handle return
    $ intercept (\(Writer l k) -> tell (f l) >> k)
    $ defaultRelay

runWriter :: EffectWriter w es => Effect (Writer w ': es) a -> Effect es (a, w)
runWriter =
    handle point
    $ eliminate bind
    $ defaultRelay

point :: EffectWriter w es => a -> Effect es (a, w)
point x = return (x, mempty)

bind :: Monoid w => Writer w (Effect es (b, w)) -> Effect es (b, w)
bind (Writer l k) = second (`mappend` l) <$> k
