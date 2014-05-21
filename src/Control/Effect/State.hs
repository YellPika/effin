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

module Control.Effect.State (
    EffectState, State, runState,
    evalState, execState,
    get, gets, put,
    modify, modify',
    state, withState
) where

import Control.Applicative ((<$>))
import Control.Monad.Effect

#ifdef MTL
import qualified Control.Monad.State.Class as S
import Data.Type.Row

instance (Member (State s) l, State s ~ InstanceOf State l) => S.MonadState s (Effect l) where
    get = get
    put = put
    state = state
#endif

-- | An effect where a state value is threaded throughout the computation.
newtype State s a = State (s -> (a, s))
  deriving Functor

type instance Is State f = IsState f

type family IsState f where
    IsState (State s) = True
    IsState f = False

class MemberEffect State (State s) l => EffectState s l
instance MemberEffect State (State s) l => EffectState s l

-- | Gets the current state.
get :: EffectState s l => Effect l s
get = state $ \s -> (s, s)

-- | Gets a value that is a function of the current state.
gets :: EffectState s l => (s -> a) -> Effect l a
gets f = f <$> get

-- | Replaces the current state.
put :: EffectState s l => s -> Effect l ()
put x = state $ const ((), x)

-- | Applies a pure modifier to the state value.
modify :: EffectState s l => (s -> s) -> Effect l ()
modify f = get >>= put . f

-- | Applies a pure modifier to the state value.
-- The modified value is converted to weak head normal form.
modify' :: EffectState s l => (s -> s) -> Effect l ()
modify' f = do
    x <- get
    put $! f x

-- | Lifts a stateful computation to the `Effect` monad.
state :: EffectState s l => (s -> (a, s)) -> Effect l a
state = send . State

-- | Runs a computation with a modified state value.
--
-- prop> withState f x = modify f >> x
withState :: EffectState s l => (s -> s) -> Effect l a -> Effect l a
withState f x = modify f >> x

-- | Completely handles a `State` effect by providing an
-- initial state, and making the final state explicit.
runState :: s -> Effect (State s :+ l) a -> Effect l (a, s)
runState = flip $ eliminate
    (\x s -> return (x, s))
    (\(State k) s -> let (k', s') = k s in k' s')

-- | Completely handles a `State` effect, and discards the final state.
evalState :: s -> Effect (State s :+ l) a -> Effect l a
evalState s = fmap fst . runState s

-- | Completely handles a `State` effect, and discards the final value.
execState :: s -> Effect (State s :+ l) a -> Effect l s
execState s = fmap snd . runState s
