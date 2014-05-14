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
    EffectException, Exception, runException,
    raise, except
) where

import Control.Effect.Bracket
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

-- | Completely handles an exception effect.
runException :: (EffectBracket s es, Show e) => Effect (Exception e ': es) a -> Effect es (Either e a)
runException effect = do
    tag <- newTag show
    exceptWith tag (run tag effect) (return . Left)
  where
    run tag =
        handle (return . Right)
        $ eliminate (bind tag)
        $ defaultRelay

    bind tag (Raise e) = raiseWith tag e
    bind tag (Catch x f) = exceptWith tag x f
