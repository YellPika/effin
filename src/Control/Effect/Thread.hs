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
import Control.Monad (void, unless)
import Control.Effect.Monad (runMonad)
import Control.Monad.Effect (Effect, Member, send, handle, eliminate, defaultRelay)

data Thread a where
    Yield :: Thread ()
    Fork :: Thread Bool
    Abort :: Thread a

type EffectThread = Member Thread

yield :: EffectThread es => Effect es ()
yield = send Yield

fork :: EffectThread es => Effect es () -> Effect es ()
fork child = do
    isParent <- send Fork
    unless isParent $ do
        child
        abort

abort :: EffectThread es => Effect es ()
abort = send Abort

-- | Executes a threaded computation synchronously.
-- All threads exit when the main thread exits.
runMain :: Effect (Thread ': es) () -> Effect es ()
runMain = run [] . reifyThreads
  where
    run xs thread = do
        result <- thread
        case result of
            Abort' -> return ()
            Yield' x -> do
                xs' <- runAll xs
                run xs' x
            Fork' x y -> do
                xs' <- runAll [x]
                run (xs ++ xs') y

    runAll [] = return []
    runAll (thread:xs) = do
        result <- thread
        case result of
            Abort' -> runAll xs
            Yield' x -> (x:) <$> runAll xs
            Fork' x y -> (y:) <$> runAll (x:xs)

-- | Executes a threaded computation synchronously.
-- Does not complete until all threads have exited.
runSync :: Effect (Thread ': es) () -> Effect es ()
runSync = run . (:[]) . reifyThreads
  where
    run [] = return ()
    run (thread:xs) = do
        result <- thread
        case result of
            Abort' -> run xs
            Yield' x -> run (xs ++ [x])
            Fork' x y -> run (x:xs ++ [y])

-- | Executes a threaded computation asynchronously.
runAsync :: Effect '[Thread, IO] () -> IO ()
runAsync = run . reifyThreads
  where
    run thread = do
        result <- runMonad thread
        case result of
            Abort' -> return ()
            Yield' x -> do
                IO.yield
                run x
            Fork' x y -> do
                void $ IO.forkIO $ run x
                run y

data ThreadAST es
    = Yield' (Effect es (ThreadAST es))
    | Fork' (Effect es (ThreadAST es)) (Effect es (ThreadAST es))
    | Abort'

-- Converts a threaded computation into a corresponding AST. This allows
-- different backends to interpret calls to fork/yield/abort as they please. See
-- the implementations of runAsync, runSync, and runMain.
reifyThreads :: Effect (Thread ': es) () -> Effect es (ThreadAST es)
reifyThreads =
    handle (\() -> return Abort')
    $ eliminate bind
    $ defaultRelay
  where
    bind :: (c -> Effect es (ThreadAST es)) -> Thread c -> Effect es (ThreadAST es)
    bind _ Abort = return Abort'
    bind k Yield = return $ Yield' (k ())
    bind k Fork = return $ Fork' (k False) (k True)
