{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Effect.State (
    EffectState, State, runState,
    evalState, execState,
    get, gets, put,
    modify, modify',
    state, withState
) where

import Control.Applicative ((<$>))
import Control.Monad.Effect (Effect, Member, send, sendEffect, handle, eliminate, relay)

-- | An effect where a state value is threaded throughout the computation.
newtype State s a = State (s -> (s, a))
  deriving Functor

type EffectState s es = (Member (State s) es, s ~ StateType es)
type family StateType es where
    StateType (State s ': es) = s
    StateType (e ': es) = StateType es

-- | Gets the current state.
get :: EffectState s es => Effect es s
get = state $ \s -> (s, s)

-- | Gets a value that is a function of the current state.
gets :: EffectState s es => (s -> a) -> Effect es a
gets f = f <$> get

-- | Replaces the current state.
put :: EffectState s es => s -> Effect es ()
put x = state $ const (x, ())

-- | Applies a pure modifier to the state value.
modify :: EffectState s es => (s -> s) -> Effect es ()
modify f = get >>= put . f

-- | Applies a pure modifier to the state value.
-- The modified value is converted to weak head normal form.
modify' :: EffectState s es => (s -> s) -> Effect es ()
modify' f = do
    x <- get
    put $! f x

-- | Lifts a stateful computation to the `Effect` monad.
state :: EffectState s es => (s -> (s, a)) -> Effect es a
state = send . State

-- | Runs a computation with a modified state value.
--
-- prop> withState f x = modify f >> x
withState :: EffectState s es => (s -> s) -> Effect es a -> Effect es a
withState f x = modify f >> x

-- | Completely handles a `State` effect by providing an
-- initial state, and making the final state explicit.
runState :: s -> Effect (State s ': es) a -> Effect es (s, a)
runState = flip $
    handle (\x s -> return (s, x))
    $ eliminate (\(State k) s -> let (s', k') = k s in k' s')
    $ relay (\x s -> sendEffect $ fmap ($ s) x)

-- | Completely handles a `State` effect, and discards the final state.
evalState :: s -> Effect (State s ': es) a -> Effect es a
evalState s = fmap snd . runState s

-- | Completely handles a `State` effect, and discards the final value.
execState :: s -> Effect (State s ': es) a -> Effect es s
execState s = fmap fst . runState s
