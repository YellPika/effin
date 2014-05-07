{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Control.Effect.Thread (
    Thread, EffectThread,
    runSync, runAsync,
    yield, fork, abort,
) where

import qualified Control.Concurrent as IO
import Control.Monad (void)
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
    parent <- send Fork
    if parent
    then return ()
    else child >> abort

abort :: EffectThread es => Effect es ()
abort = send Abort

runSync :: Effect (Thread ': es) () -> Effect es ()
runSync = run . (:[]) . runThread'
  where
    run [] = return ()
    run (thread:xs) = do
        result <- thread
        case result of
            Abort' -> run xs
            Yield' x -> run (xs ++ [x])
            Fork' x y -> run (x:xs ++ [y])

runAsync :: Effect '[Thread, IO] () -> IO ()
runAsync = run . runThread'
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

data Thread' es
    = Yield' (Effect es (Thread' es))
    | Fork' (Effect es (Thread' es)) (Effect es (Thread' es))
    | Abort'

runThread' :: Effect (Thread ': es) () -> Effect es (Thread' es)
runThread' =
    handle (\() -> return Abort')
    $ eliminate bind
    $ defaultRelay
  where
    bind :: (c -> Effect es (Thread' es)) -> Thread c -> Effect es (Thread' es)
    bind _ Abort = return Abort'
    bind k Yield = return $ Yield' (k ())
    bind k Fork = return $ Fork' (k False) (k True)
