{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module River.Core.Color (
    ColorStrategy(..)
  , colorByInt

  , coloredOfProgram
  , precoloredOfProgram

  , colorsOfProgram

  , ColorError(..)
  ) where

import           Control.Monad (foldM)

import           Data.Bifunctor (first)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Void (Void)

import           River.Core.Analysis.Interference
import           River.Core.Fresh
import           River.Core.Syntax
import           River.Fresh
import           River.Map

------------------------------------------------------------------------

data ColorError e n =
    MissingFromInterference !n
  | MissingFromBindings !n
  | StrategyError !e
    deriving (Eq, Ord, Show)

data ColorStrategy e c k p n a =
  ColorStrategy {
      -- | Given the binding and the set of colors in use by neighbors, return
      --   the color to assign to the current variable.
      unusedColor :: n -> Set c -> Either e c

      -- | Given a program, find the names that are pre-colored.
    , precolored :: FreshName n => Program k p n a -> Fresh (Map n c, Program k p n a)
    }

-- | Simple coloring strategy which colors the graph using integers.
colorByInt :: ColorStrategy Void Int k p n a
colorByInt =
  ColorStrategy {
      unusedColor =
        \_ used ->
          pure . head $ filter (not . flip Set.member used) [0..]
    , precolored =
        \p ->
          pure (Map.empty, p)
    }

-- | Rename variables to their optimal K-coloring.
coloredOfProgram ::
  Ord c =>
  Ord n =>
  FreshName n =>
  ColorStrategy e c k p n a ->
  Program k p n a ->
  Either (ColorError e n) (Program k p (n, Maybe c) a)
coloredOfProgram strategy p0 = do
  let
    lookupName colors n =
      case Map.lookup n colors of
        Nothing ->
          (n, Nothing)
        Just x ->
          (n, Just x)
  (colors, p) <- colorsOfProgram strategy p0
  pure $ first (lookupName colors) p

-- | Rename variables to their potentially pre-colored names.
precoloredOfProgram ::
  Ord n =>
  FreshName n =>
  ColorStrategy e c k p n a ->
  Program k p n a ->
  Program k p (n, Maybe c) a
precoloredOfProgram strategy p0 =
  let
    (colors, p) =
      runFreshFrom (nextOfProgram p0) $
      precolored strategy p0

    lookupName n =
      case Map.lookup n colors of
        Nothing ->
          (n, Nothing)
        Just x ->
          (n, Just x)
  in
    first lookupName p

------------------------------------------------------------------------

-- | Find the optimal K-coloring for the variables in a program.
--
--   Simplical elimination ordering method.
--
colorsOfProgram ::
  forall e c k p n a.
  Ord c =>
  Ord n =>
  FreshName n =>
  ColorStrategy e c k p n a ->
  Program k p n a ->
  Either (ColorError e n) (Map n c, Program k p n a)
colorsOfProgram strategy p0 = do
  let
    (colors0, p) =
      runFreshFrom (nextOfProgram p0) $
      precolored strategy p0

    interference :: InterferenceGraph n
    interference =
      interferenceOfProgram p

    ordering :: [n]
    ordering =
      -- simplicial interference
      orderingOfProgram p

    go :: Map n c -> n -> Either (ColorError e n) (Map n c)
    go colors n = do
      if Map.member n colors then
        -- pre-colored node
        pure colors
      else do
        ns <-
          maybe (Left $ MissingFromInterference n) pure $
          neighbors n interference

        let
          used =
            Set.fromList . Map.elems $
            colors `mapIntersectionSet` ns

        color <-
          first StrategyError $
          unusedColor strategy n used

        pure $
          Map.insert n color colors

  colors <- foldM go colors0 ordering
  pure (colors, p)

------------------------------------------------------------------------

-- | Alternative to River.Core.Analysis.Simplicial.
--
--   If I've understood [1] correctly, this should create a simplicial ordering
--   also. My assumption is that a pre-order traversal of an SSA dominator tree
--   is equivalent to a pre-order traversal of a program in ANF.
--
--   Spoke to Patryk regarding dominator tree traversal vs ANF traversal, they
--   should be equivalent as long as lambda dropping has been applied to the
--   ANF program.
--
--   1. Sebastian Hack. Register Allocation for Programs in SSA Form, 2007
--
orderingOfProgram :: Program k p n a -> [n]
orderingOfProgram = \case
  Program _ tm ->
    orderingOfTerm tm

orderingOfTerm :: Term k p n a -> [n]
orderingOfTerm = \case
  Return _ _ ->
    []
  If _ _ _ t e ->
    orderingOfTerm t ++
    orderingOfTerm e
  Let _ ns _ tm ->
    ns ++
    orderingOfTerm tm
  LetRec _ bs tm ->
    orderingOfBindings bs ++
    orderingOfTerm tm

orderingOfBindings :: Bindings k p n a -> [n]
orderingOfBindings = \case
  Bindings _ bs ->
    concatMap (orderingOfBinding . snd) bs

orderingOfBinding :: Binding k p n a -> [n]
orderingOfBinding = \case
  Lambda _ ns tm ->
    ns ++
    orderingOfTerm tm
