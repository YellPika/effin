{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
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
import Data.Type.Row
import qualified Control.Monad.Error.Class as E

instance (Member (Exception e) l, Exception e ~ InstanceOf Exception l) => E.MonadError e (Effect l) where
    throwError = raise
    catchError = except
#endif

-- | An effect that describes the possibility of failure.
data Exception e a = Raise e | Catch a (e -> a)

type instance Is Exception f = IsException f

type family IsException f where
    IsException (Exception e) = True
    IsException f = False

class MemberEffect Exception (Exception e) l => EffectException e l
instance MemberEffect Exception (Exception e) l => EffectException e l

-- | Raises an exception.
raise :: EffectException e l => e -> Effect l a
raise = send . Raise

-- | Handles an exception. Intended to be used in infix form.
--
-- > myComputation `except` \ex -> doSomethingWith ex
except :: EffectException e l => Effect l a -> (e -> Effect l a) -> Effect l a
except x f = sendEffect (Catch x f)

-- | Completely handles an exception effect.
runException :: (EffectBracket s l, Show e) => Effect (Exception e :+ l) a -> Effect l (Either e a)
runException effect = do
    tag <- newTag show
    exceptWith tag
        (eliminate (return . Right) (bind tag) effect)
        (return . Left)
  where
    bind tag (Raise e) _ = raiseWith tag e
    bind tag (Catch x f) k = exceptWith tag (k x) (k . f)
