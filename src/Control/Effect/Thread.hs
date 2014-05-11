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

class Member Thread es => EffectThread es
instance Member Thread es => EffectThread es

-- | Yields to the next available thread.
yield :: EffectThread es => Effect es ()
yield = send (Yield ())

-- | Forks a child thread.
fork :: EffectThread es => Effect es () -> Effect es ()
fork child = sendEffect $ Fork child (return ())

-- | Immediately terminates the current thread.
abort :: EffectThread es => Effect es ()
abort = send Abort

-- | Executes a threaded computation synchronously.
-- Completes when the main thread exits.
runMain :: Effect (Thread ': es) () -> Effect es ()
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
runSync :: Effect (Thread ': es) () -> Effect es ()
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
runAsync :: Effect '[Thread, Lift IO] () -> IO ()
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

data ThreadAST es
    = YieldAST (Effect es (ThreadAST es))
    | ForkAST (Effect es (ThreadAST es)) (Effect es (ThreadAST es))
    | AbortAST

-- Converts a threaded computation into its corresponding AST. This allows
-- different backends to interpret calls to fork/yield/abort as they please. See
-- the implementations of runAsync, runSync, and runMain.
toAST :: Effect (Thread ': es) () -> Effect es (ThreadAST es)
toAST =
    handle (\() -> return AbortAST)
    $ eliminate (\thread ->
        case thread of
            Abort -> return AbortAST
            Yield k -> return (YieldAST k)
            Fork child parent -> return (ForkAST child parent))
    $ defaultRelay
