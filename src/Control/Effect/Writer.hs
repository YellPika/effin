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

module Control.Effect.Writer (
    EffectWriter, Writer, runWriter,
    tell, listen, listens, pass, censor,
    stateWriter
) where

import Control.Monad.Effect
import Control.Applicative ((<$>))
import Control.Arrow (second)
import Data.Monoid (Monoid (..))
import Control.Effect.State

#ifdef MTL
import Data.Type.Row
import qualified Control.Monad.Writer.Class as W

instance (Monoid w, Member (Writer w) l, Writer w ~ InstanceOf Writer l) => W.MonadWriter w (Effect l) where
    tell = tell
    listen = listen
    pass = pass
#endif

-- | An effect that allows accumulating output.
data Writer w a = Writer w a
  deriving Functor

type instance Is Writer f = IsWriter f

type family IsWriter f where
    IsWriter (Writer w) = True
    IsWriter f = False

class (Monoid w, MemberEffect Writer (Writer w) l) => EffectWriter w l
instance (Monoid w, MemberEffect Writer (Writer w) l) => EffectWriter w l

-- | Writes a value to the output.
tell :: EffectWriter w l => w -> Effect l ()
tell x = send (Writer x ())

-- | Executes a computation, and obtains the writer output.
-- The writer output of the inner computation is still
-- written to the writer output of the outer computation.
listen :: EffectWriter w l => Effect l a -> Effect l (a, w)
listen effect = do
    value@(_, output) <- intercept point bind effect
    tell output
    return value

-- | Like `listen`, but the writer output is run through a function.
listens :: EffectWriter w l => (w -> b) -> Effect l a -> Effect l (a, b)
listens f = fmap (second f) . listen

-- | Runs a computation that returns a value and a function,
-- applies the function to the writer output, and then returns the value.
pass :: EffectWriter w l => Effect l (a, w -> w) -> Effect l a
pass effect = do
    ((x, f), l) <- listen effect
    tell (f l)
    return x

-- | Applies a function to the writer output of a computation.
censor :: EffectWriter w l => (w -> w) -> Effect l a -> Effect l a
censor f effect = pass $ do
    a <- effect
    return (a, f)

-- | Executes a writer computation which sends its output to a state effect.
stateWriter :: (Monoid s, EffectState s l) => Effect (Writer s :+ l) a -> Effect l a
stateWriter = eliminate return (\(Writer l x) -> modify (mappend l) >> x)

-- | Completely handles a writer effect. The writer value must be a `Monoid`.
-- `mempty` is used as an initial value, and `mappend` is used to combine values.
-- Returns the result of the computation and the final output value.
runWriter :: Monoid w => Effect (Writer w :+ l) a -> Effect l (a, w)
runWriter = eliminate point bind

point :: Monoid w => a -> Effect l (a, w)
point x = return (x, mempty)

bind :: Monoid w => Writer w (Effect l (b, w)) -> Effect l (b, w)
bind (Writer l k) = second (mappend l) <$> k
