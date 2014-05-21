{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Effect.List (
    EffectList, List, runList,
    choose, never, select,

    CutEffect, Cut, runCut,
    cut, cutFalse
) where

import Control.Monad.Effect
import Control.Arrow (second)
import Control.Applicative (Alternative (..), (<$>))
import Control.Monad (MonadPlus (..), (<=<), join)

-- | A nondeterminism (backtracking) effect.
newtype List a = List { unList :: [a] }
  deriving Functor

type instance Is List f = IsList f

type family IsList f where
    IsList List = True
    IsList f = False

class Member List l => EffectList l
instance Member List l => EffectList l

-- | Nondeterministically chooses a value from the input list.
choose :: EffectList l => [a] -> Effect l a
choose = send . List

-- | Describes a nondeterministic computation that never returns a value.
never :: EffectList l => Effect l a
never = choose []

-- | Nondeterministically chooses a value from a list of computations.
select :: EffectList l => [Effect l a] -> Effect l a
select = join . choose

-- | Obtains all possible values from a computation
-- parameterized by a nondeterminism effect.
runList :: Effect (List :+ l) a -> Effect l [a]
runList = eliminate (return . return) (fmap concat . sequence . unList)

instance EffectList l => Alternative (Effect l) where
    empty = never
    x <|> y = select [x, y]

instance EffectList l => MonadPlus (Effect l) where
    mzero = empty
    mplus = (<|>)

-- | Describes a Prolog-like cut effect.
-- This effect must be used with the `List` effect.
data Cut a = CutFalse
  deriving Functor

class (EffectList l, Member Cut l) => CutEffect l
instance (EffectList l, Member Cut l) => CutEffect l

-- | Prevents backtracking past the point this value was invoked,
-- in the style of Prolog's "!" operator.
cut :: CutEffect l => Effect l ()
cut = return () <|> cutFalse

-- | Prevents backtracking past the point this value was invoked.
-- Unlike Prolog's "!" operator, `cutFalse` will cause the current
-- computation to fail immediately, instead of when it backtracks.
cutFalse :: CutEffect l => Effect l a
cutFalse = send CutFalse

-- | Handles the `Cut` effect. `cut`s have no effect beyond
-- the scope of the computation passed to this function.
runCut :: EffectList l => Effect (Cut :+ l) a -> Effect l a
runCut = choose . snd <=< reifyCut
  where
    -- Gather the results of a computation into a list (like in runList), but
    -- also return a Bool indicating whether a cut was performed in the
    -- computation. When we intercept the List effect, we get a continuation and a
    -- list of values. If we map the continuation to the list of values, then we
    -- get a list of computations. We can now execute each computation one by
    -- one, and inspect the Bool after each computation to determine when we
    -- should stop.
    reifyCut :: EffectList l => Effect (Cut :+ l) a -> Effect l (Bool, [a])
    reifyCut =
        intercept return (runAll . unList) .
        eliminate
            (\x -> return (False, [x]))
            (\CutFalse -> return (True, []))

    runAll [] = return (False, [])
    runAll (x:xs) = do
        (cutRequested, x') <- x
        if cutRequested
        then return (True, x')
        else second (x' ++) <$> runAll xs
