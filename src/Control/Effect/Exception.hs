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

module Control.Effect.Exception (
    EffectException, Exception, runException, runIOException,
    raise, except, finally
) where

import Control.Exception (SomeException (..), catch, throwIO)
import Control.Effect.Lift
import Control.Monad.Effect

#ifdef MTL
import qualified Control.Monad.Error.Class as E

instance (Member (Exception e) es, e ~ ExceptionType es) => E.MonadError e (Effect es) where
    throwError = raise
    catchError = except
#endif

-- | An effect that describes the possibility of failure.
data Exception e a = Raise e | Catch a (e -> a)
  deriving Functor

class (Member (Exception e) es, e ~ ExceptionType es) => EffectException e es
instance (Member (Exception e) es, e ~ ExceptionType es) => EffectException e es

type family ExceptionType es where
    ExceptionType (Exception e ': es) = e
    ExceptionType (e ': es) = ExceptionType es

-- | Raises an exception.
raise :: EffectException e es => e -> Effect es a
raise = send . Raise

-- | Handles an exception. Intended to be used in infix form.
--
-- > myComputation `except` \ex -> doSomethingWith ex
except :: EffectException e es => Effect es a -> (e -> Effect es a) -> Effect es a
except x f = sendEffect (Catch x f)

-- | Ensures that a computation is run after another one completes,
-- regardless of whether an exception was raised. Intended to be
-- used in infix form.
--
-- > do x <- loadSomeResource
-- >    doSomethingWith x `finally` unload x
finally :: EffectException e es => Effect es a -> Effect es () -> Effect es a
finally effect finalizer = do
    result <- effect `except` \e -> do
        finalizer
        raise e
    finalizer
    return result

-- | Completely handles an exception effect.
runException :: Effect (Exception e ': es) a -> Effect es (Either e a)
runException =
    handle point
    $ eliminate bind
    $ defaultRelay
  where
    point = return . Right

    bind (Raise e) = return (Left e)
    bind (Catch x f) = x >>= either f point

runIOException :: EffectLift IO es => Effect (Exception SomeException ': es) a -> Effect es (Either SomeException a)
runIOException =
    ( handle (return . Right)
    $ intercept (\(Lift m) -> liftEffect $ m `catch` (return . return . Left))
    $ defaultRelay
    ) . run
  where
    run =
        handle return
        $ eliminate bind
        $ defaultRelay

    bind (Raise (SomeException e)) = lift $ throwIO e
    bind (Catch x f) = runCatch f x

    runCatch handler =
        handle return
        $ intercept (\(Lift m) -> liftEffect $ m `catch` (return . handler))
        $ defaultRelay
