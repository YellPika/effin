{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Control.Effect.Thread (
    Thread, EffectThread,
    runMain, runSync, runAsync,
    yield, fork, abort,
) where

import qualified Control.Concurrent as IO
import Control.Applicative ((<$>))
import Control.Monad (void)
import Control.Effect.Monad (runMonad)
import Control.Monad.Effect (Effect, Member, send, handle, eliminate, defaultRelay)

data Thread a
    = Yield a
    | Fork a a
    | Abort

type EffectThread = Member Thread

yield :: EffectThread es => Effect es ()
yield = send $ Yield $ return ()

fork :: EffectThread es => Effect es () -> Effect es ()
fork child = send $ Fork child $ return ()

abort :: EffectThread es => Effect es ()
abort = send Abort

-- | Executes a threaded computation synchronously.
-- All threads exit when the main thread exits.
runMain :: Effect (Thread ': es) () -> Effect es ()
runMain = run [] . reifyThreads
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
runSync = run . (:[]) . reifyThreads
  where
    run [] = return ()
    run (thread:xs) = do
        result <- thread
        case result of
            AbortAST -> run xs
            YieldAST k -> run (xs ++ [k])
            ForkAST child parent -> run (child:xs ++ [parent])

-- | Executes a threaded computation asynchronously.
runAsync :: Effect '[Thread, IO] () -> IO ()
runAsync = run . reifyThreads
  where
    run thread = do
        result <- runMonad thread
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

-- Converts a threaded computation into a corresponding AST. This allows
-- different backends to interpret calls to fork/yield/abort as they please. See
-- the implementations of runAsync, runSync, and runMain.
reifyThreads :: Effect (Thread ': es) () -> Effect es (ThreadAST es)
reifyThreads =
    handle (\() -> return AbortAST)
    $ eliminate (\thread ->
        case thread of
            Abort -> return AbortAST
            Yield k -> return (YieldAST k)
            Fork child parent -> return (ForkAST child parent))
    $ defaultRelay
