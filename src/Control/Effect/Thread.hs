{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.Thread (
    EffectThread, Thread,
    runMain, runSync, runAsync,
    yield, fork, abort,
) where

import Control.Effect.Lift
import Control.Monad.Effect
import Control.Applicative ((<$>))
import Control.Monad (void)
import qualified Control.Concurrent as IO

-- | An effect that describes concurrent computation.
data Thread a = Yield a | Fork a a | Abort
  deriving Functor

class Member Thread l => EffectThread l
instance Member Thread l => EffectThread l

-- | Yields to the next available thread.
yield :: EffectThread l => Effect l ()
yield = send (Yield ())

-- | Forks a child thread.
fork :: EffectThread l => Effect l () -> Effect l ()
fork child = sendEffect $ Fork child (return ())

-- | Immediately terminates the current thread.
abort :: EffectThread l => Effect l ()
abort = send Abort

-- | Executes a threaded computation synchronously.
-- Completes when the main thread exits.
runMain :: Effect (Thread :+ l) () -> Effect l ()
runMain = run [] . toAST
  where
    run auxThreads thread = do
        result <- thread
        case result of
            AbortAST -> return ()
            YieldAST k -> do
                auxThreads' <- runAll auxThreads
                run auxThreads' k
            ForkAST child parent -> do
                auxThreads' <- runAll [child]
                run (auxThreads ++ auxThreads') parent

    runAll [] = return []
    runAll (thread:xs) = do
        result <- thread
        case result of
            AbortAST -> runAll xs
            YieldAST k -> (k:) <$> runAll xs
            ForkAST child parent -> (parent:) <$> runAll (child:xs)

-- | Executes a threaded computation synchronously.
-- Does not complete until all threads have exited.
runSync :: Effect (Thread :+ l) () -> Effect l ()
runSync = run . (:[]) . toAST
  where
    run [] = return ()
    run (thread:xs) = do
        result <- thread
        case result of
            AbortAST -> run xs
            YieldAST k -> run (xs ++ [k])
            ForkAST child parent -> run (child:xs ++ [parent])

-- | Executes a threaded computation asynchronously.
runAsync :: Effect (Thread :+ Lift IO :+ Nil) () -> IO ()
runAsync = run . toAST
  where
    run thread = do
        result <- runLift thread
        case result of
            AbortAST -> return ()
            YieldAST k -> do
                IO.yield
                run k
            ForkAST child parent -> do
                void $ IO.forkIO $ run child
                run parent

data ThreadAST l
    = YieldAST (Effect l (ThreadAST l))
    | ForkAST (Effect l (ThreadAST l)) (Effect l (ThreadAST l))
    | AbortAST

-- Converts a threaded computation into its corresponding AST. This allows
-- different backends to interpret calls to fork/yield/abort as they please. See
-- the implementations of runAsync, runSync, and runMain.
toAST :: Effect (Thread :+ l) () -> Effect l (ThreadAST l)
toAST = eliminate (\() -> return AbortAST) bind
  where
    bind Abort = return AbortAST
    bind (Yield k) = return (YieldAST k)
    bind (Fork child parent) = return (ForkAST child parent)
