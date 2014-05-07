{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Effect.Exception (
    EffectException, Exception, runException,
    raise, except, finally
) where

import Control.Monad.Effect (Effect, Member, send, handle, eliminate, intercept, defaultRelay)

newtype Exception e a = Exception { unException :: e }
  deriving Functor

type EffectException e es = (Member (Exception e) es, e ~ ExceptionType es)
type family ExceptionType es where
    ExceptionType (Exception e ': es) = e
    ExceptionType (e ': es) = ExceptionType es

raise :: EffectException e es => e -> Effect es a
raise = send . Exception

except :: EffectException e es => Effect es a -> (e -> Effect es a) -> Effect es a
except = flip run
  where
    run handler =
        handle return
        $ intercept (handler . unException)
        $ defaultRelay

finally :: EffectException e es => Effect es a -> Effect es () -> Effect es a
finally effect finalizer = do
    result <- effect `except` \e -> do
        finalizer
        raise e
    finalizer
    return result

runException :: Effect (Exception e ': es) a -> Effect es (Either e a)
runException =
    handle (return . Right)
    $ eliminate (return . Left . unException)
    $ defaultRelay
