{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Effect.List (
    EffectList, List, runList,
    choose, never, select,

    EffectCut, Cut,
    cut, runCut
) where

import Control.Arrow (second)
import Control.Applicative (Alternative (..), (<$>))
import Control.Monad (MonadPlus (..), (<=<), join)
import Control.Monad.Effect (Effect, send, handle, eliminate, intercept, defaultRelay)
import Data.Member (Member)

-- | Describes a nondeterminism (backtracking) effect.
newtype List a = List { unList :: [a] }
  deriving Functor

type EffectList = Member List

-- | Nondeterministically chooses a value from the input list.
choose :: EffectList es => [a] -> Effect es a
choose = send . List

-- | Describes a nondeterministic computation that never returns a value.
never :: EffectList es => Effect es a
never = choose []

-- | Nondeterministically chooses a value from a list of computations.
select :: EffectList es => [Effect es a] -> Effect es a
select = join . choose

-- | Obtains all possible values from a computation
-- parameterized by a nondeterminism effect.
runList :: Effect (List ': es) a -> Effect es [a]
runList =
    handle (\x -> return [x])
    $ eliminate (fmap concat . sequence . unList)
    $ defaultRelay

instance EffectList es => Alternative (Effect es) where
    empty = never
    x <|> y = select [x, y]

instance EffectList es => MonadPlus (Effect es) where
    mzero = empty
    mplus = (<|>)

-- | Describes a Prolog-like cut effect.
-- This effect must be used with the `List` effect.
data Cut a = Cut
  deriving Functor

type EffectCut = Member Cut

-- | Prevents backtracking past the point this value was invoked.
-- Unlike Prolog's '!' operator, `cut` will cause the current
-- computation to fail immediately, instead of when it backtracks.
cut :: (EffectList es, EffectCut es) => Effect es a
cut = send Cut

-- | Handles the `Cut` effect. `cut`s have no effect beyond
-- the scope of the computation passed to this function.
runCut :: EffectList es => Effect (Cut ': es) a -> Effect es a
runCut = choose . snd <=< reifyCut
  where
    -- Gather the results of a computation into a list (like in runList), but
    -- also return a Bool indicating whether a cut was performed in the
    -- computation. When we intercept the List effect, we get a continuation and
    -- a list of values. If we map the continuation to the list of values, then
    -- we get a list of computations. We can now execute each computation one by
    -- one, and inspect the Bool after each computation to determine when we
    -- should stop.
    reifyCut :: EffectList es => Effect (Cut ': es) a -> Effect es (Bool, [a])
    reifyCut =
        handle (\x -> return (False, [x]))
        $ eliminate (\Cut -> return (True, []))
        $ intercept (\(List xs) -> runAll xs)
        $ defaultRelay

    runAll [] = return (False, [])
    runAll (x:xs) = do
        (cutRequested, x') <- x
        if cutRequested
        then return (True, x')
        else second (x' ++) <$> runAll xs
