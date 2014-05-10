{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

#if MTL
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

module Control.Effect.Exception (
    EffectException, Exception, runException,
    raise, except, finally
) where

import Control.Monad.Effect

#ifdef MTL
import qualified Control.Monad.Error.Class as E

instance EffectException e es => E.MonadError e (Effect es) where
    throwError = raise
    catchError = except
#endif

-- | An effect that describes the possibility of failure.
newtype Exception e a = Exception { unException :: e }
  deriving Functor

type EffectException e es = (Member (Exception e) es, e ~ ExceptionType es)
type family ExceptionType es where
    ExceptionType (Exception e ': es) = e
    ExceptionType (e ': es) = ExceptionType es

-- | Raises an exception.
raise :: EffectException e es => e -> Effect es a
raise = send . Exception

-- | Handles an exception. Intended to be used in infix form.
--
-- > myComputation `except` \ex -> doSomethingWith ex
except :: EffectException e es => Effect es a -> (e -> Effect es a) -> Effect es a
except = flip run
  where
    run handler =
        handle return
        $ intercept (handler . unException)
        $ defaultRelay

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
    handle (return . Right)
    $ eliminate (return . Left . unException)
    $ defaultRelay
