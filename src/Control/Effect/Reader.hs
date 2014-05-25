{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
import Control.Applicative ((<$>))

#ifdef MTL
import Data.Type.Row
import qualified Control.Monad.Reader.Class as R

instance (Member (Reader r) l, Reader r ~ InstanceOf Reader l) => R.MonadReader r (Effect l) where
    ask = ask
    local = local
    reader = asks
#endif

-- | An effect that provides an implicit environment.
data Reader r a where
    Ask :: Reader r r

type instance Is Reader f = IsReader f

type family IsReader f where
    IsReader (Reader r) = True
    IsReader f = False

class MemberEffect Reader (Reader r) l => EffectReader r l
instance MemberEffect Reader (Reader r) l => EffectReader r l

-- | Retrieves the current environment.
ask :: EffectReader r l => Effect l r
ask = send Ask

-- | Retrieves a value that is a function of the current environment.
asks :: EffectReader r l => (r -> a) -> Effect l a
asks f = f <$> ask

-- | Runs a computation with a modified environment.
local :: EffectReader r l => (r -> r) -> Effect l a -> Effect l a
local f effect = do
    env <- asks f
    intercept return (bind env) effect

-- | Executes a reader computation which obtains
-- its environment value from a state effect.
stateReader :: EffectState s l => Effect (Reader s :+ l) a -> Effect l a
stateReader = eliminate return (\Ask k -> get >>= k)

-- | Completely handles a `Reader` effect by providing an
-- environment value to be used throughout the computation.
runReader :: r -> Effect (Reader r :+ l) a -> Effect l a
runReader env = eliminate return (bind env)

bind :: r -> Reader r a -> (a -> s) -> s
bind env Ask k = k env
