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

instance (EffectBracket s l, Member (Exception s e) l, Exception s e ~ InstanceOf Exception l) => E.MonadError e (Effect l) where
    throwError = raise
    catchError = except
#endif

-- | An effect that describes the possibility of failure.
newtype Exception s e a = Exception (Tag s e -> a)

type instance Is Exception f = IsException f

type family IsException f where
    IsException (Exception s e) = 'True
    IsException f = 'False

class (EffectBracket s l, MemberEffect Exception (Exception s e) l) => EffectException s e l
instance (EffectBracket s l, MemberEffect Exception (Exception s e) l) => EffectException s e l

-- | Raises an exception.
raise :: EffectException s e l => e -> Effect l a
raise e = sendEffect (Exception (\tag -> raiseWith tag e))

-- | Handles an exception. Intended to be used in infix form.
--
-- > myComputation `except` \ex -> doSomethingWith ex
except :: EffectException s e l => Effect l a -> (e -> Effect l a) -> Effect l a
except x f = sendEffect (Exception (\tag -> exceptWith tag x f))

-- | Completely handles an exception effect.
runException :: (EffectBracket s l, Show e) => Effect (Exception s e ':+ l) a -> Effect l (Either e a)
runException effect = do
    tag <- newTag show
    exceptWith tag
        (eliminate (return . Right) (\(Exception f) k -> k (f tag)) effect)
        (return . Left)
