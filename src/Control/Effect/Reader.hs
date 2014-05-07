{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Effect.Reader (
	EffectReader, Reader, runReader,
    ask, asks, local
) where

import Control.Monad.Effect (Effect, Member, send, handle, eliminate, intercept, defaultRelay)

data Reader r a = Reader (r -> a)

type EffectReader r es = (Member (Reader r) es, r ~ ReaderType es)
type family ReaderType es where
    ReaderType (Reader r ': es) = r
    ReaderType (e ': es) = ReaderType es

ask :: EffectReader r es => Effect es r
ask = asks id

asks :: EffectReader r es => (r -> a) -> Effect es a
asks = send . Reader

local :: EffectReader r es => (r -> r) -> Effect es a -> Effect es a
local f effect = do
    env <- asks f
    run env effect
  where
    run env =
        handle return
        $ intercept (bind env)
        $ defaultRelay

runReader :: r -> Effect (Reader r ': es) a -> Effect es a
runReader env =
    handle return
    $ eliminate (bind env)
    $ defaultRelay

bind :: r -> (a -> Effect es b) -> Reader r a -> Effect es b
bind env k (Reader f) = k (f env)
