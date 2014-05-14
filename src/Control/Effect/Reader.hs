{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

#if MTL
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

module Control.Effect.Reader (
	EffectReader, Reader, runReader,
    ask, asks, local,
    stateReader
) where

import Control.Effect.State
import Control.Monad.Effect

#ifdef MTL
import qualified Control.Monad.Reader.Class as R

instance (Member (Reader r) es, r ~ ReaderType es) => R.MonadReader r (Effect es) where
    ask = ask
    local = local
    reader = asks
#endif

-- | An effect that describes an implicit environment.
newtype Reader r a = Reader (r -> a)
  deriving Functor

class (Member (Reader r) es, r ~ ReaderType es) => EffectReader r es
instance (Member (Reader r) es, r ~ ReaderType es) => EffectReader r es

type family ReaderType es where
    ReaderType (Reader r ': es) = r
    ReaderType (e ': es) = ReaderType es

-- | Retrieves the current environment.
ask :: EffectReader r es => Effect es r
ask = asks id

-- | Retrieves a value that is a function of the current environment.
asks :: EffectReader r es => (r -> a) -> Effect es a
asks = send . Reader

-- | Runs a computation with a modified environment.
local :: EffectReader r es => (r -> r) -> Effect es a -> Effect es a
local f effect = do
    env <- asks f
    run env effect
  where
    run env =
        handle return
        $ intercept (bind env)
        $ defaultRelay

-- | Executes a reader computation which obtains
-- its environment value from a state effect.
stateReader :: EffectState s es => Effect (Reader s ': es) a -> Effect es a
stateReader =
    handle return
    $ eliminate (\(Reader f) -> get >>= f)
    $ defaultRelay

-- | Completely handles a `Reader` effect by providing an
-- environment value to be used throughout the computation.
runReader :: r -> Effect (Reader r ': es) a -> Effect es a
runReader env =
    handle return
    $ eliminate (bind env)
    $ defaultRelay

bind :: r -> Reader r (Effect es b) -> Effect es b
bind env (Reader k) = k env
