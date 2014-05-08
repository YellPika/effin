{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Effect.Writer (
    EffectWriter, Writer, runWriter,
    tell, listen, listens, pass, censor
) where

import Control.Applicative ((<$>))
import Control.Arrow (second)
import Control.Monad.Effect (Effect, send, handle, eliminate, intercept, defaultRelay)
import Data.Member (Member)
import Data.Monoid (Monoid (..))

-- | An effect that allows accumulating output.
data Writer w a = Writer w a
  deriving Functor

type EffectWriter w es = (Monoid w, Member (Writer w) es, w ~ WriterType es)
type family WriterType es where
    WriterType (Writer w ': es) = w
    WriterType (t ': es) = WriterType es

-- | Writes a value to the output.
tell :: EffectWriter w es => w -> Effect es ()
tell x = send (Writer x ())

-- | Executes a computation, and obtains the writer output.
-- The writer output of the inner computation is still
-- written to the writer output of the outer computation.
listen :: EffectWriter w es => Effect es a -> Effect es (a, w)
listen effect = do
    value@(_, output) <- run effect
    tell output
    return value
  where
    run =
        handle point
        $ intercept bind
        $ defaultRelay

-- | Like `listen`, but the writer output is run through a function.
listens :: EffectWriter w es => (w -> b) -> Effect es a -> Effect es (a, b)
listens f = fmap (second f) . listen

-- | Runs a computation that returns a value and a function,
-- applies the function to the writer output, and then returns the value.
pass :: EffectWriter w es => Effect es (a, w -> w) -> Effect es a
pass effect = do
    ((x, f), l) <- listen effect
    tell (f l)
    return x

-- | Applies a function to the writer output of a computation.
censor :: EffectWriter w es => (w -> w) -> Effect es a -> Effect es a
censor f =
    handle return
    $ intercept (\(Writer l k) -> tell (f l) >> k)
    $ defaultRelay

-- | Completely handles a writer effect. The writer value must be a `Monoid`.
-- `mempty` is used as an initial value, and `mappend` is used to combine values.
-- Returns the result of the computation and the final output value.
runWriter :: Monoid w => Effect (Writer w ': es) a -> Effect es (a, w)
runWriter =
    handle point
    $ eliminate bind
    $ defaultRelay

point :: Monoid w => a -> Effect es (a, w)
point x = return (x, mempty)

bind :: Monoid w => Writer w (Effect es (b, w)) -> Effect es (b, w)
bind (Writer l k) = second (mappend l) <$> k
