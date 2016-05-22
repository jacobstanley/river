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
  , bcolorsOfProgram

  , ColorError(..)
  ) where

import           Control.Monad (foldM)

import           Data.Bifunctor (first)
import           Data.Bitraversable (bitraverse)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Void (Void)

import           River.Core.Analysis.Interference
import           River.Core.Annotation
import           River.Core.Fresh
import           River.Core.Scope
import           River.Core.Syntax
import           River.Fresh

------------------------------------------------------------------------

data ColorError e n =
    MissingFromInterference !n
  | MissingFromColorMap !n
  | MissingFromBindings !n
  | StrategyError !e
    deriving (Eq, Ord, Show)

data ColorStrategy e c p n a =
  ColorStrategy {
      -- | Given the binding and the set of colors in use by neighbors, return
      --   the color to assign to the current variable.
      unusedColor :: n -> Set c -> Either e c

      -- | Given a program, find the names that are pre-colored.
    , precolored :: FreshName n => Program p n a -> Fresh (Map n c, Program p n a)
    }

-- | Simple coloring strategy which colors the graph using integers.
colorByInt :: ColorStrategy Void Int p n a
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
  ColorStrategy e c p n a ->
  Program p n a ->
  Either (ColorError e n) (Program p (n, c) a)
coloredOfProgram strategy p0 = do
  let
    lookupName colors n =
      case Map.lookup n colors of
        Nothing ->
          Left $ MissingFromColorMap n
        Just x ->
          Right (n, x)
  (colors, p) <- colorsOfProgram strategy p0
  bitraverse (lookupName colors) pure p

-- | Rename variables to their potentially pre-colored names.
precoloredOfProgram ::
  Ord c =>
  Ord n =>
  FreshName n =>
  ColorStrategy e c p n a ->
  Program p n a ->
  Program p (n, Maybe c) a
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
  forall e c p n a.
  Ord c =>
  Ord n =>
  FreshName n =>
  ColorStrategy e c p n a ->
  Program p n a ->
  Either (ColorError e n) (Map n c, Program p n a)
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
orderingOfProgram :: Program p n a -> [n]
orderingOfProgram = \case
  Program _ tm ->
    orderingOfTerm tm

orderingOfTerm :: Term p n a -> [n]
orderingOfTerm = \case
  Let _ ns _ tm ->
    ns ++ orderingOfTerm tm
  Return _ _ ->
    []

------------------------------------------------------------------------

-- | Find the optimal K-coloring for the variables in a program.
--
--   Post-order traversal method.
--
--   This doesn't seem to work when we need precolored nodes.
--
bcolorsOfProgram ::
  Ord c =>
  Ord n =>
  FreshName n =>
  ColorStrategy e c p n a ->
  Program p n a ->
  Either (ColorError e n) (Map n c, Program p n a)
bcolorsOfProgram strategy p0 = do
  case runFreshFrom (nextOfProgram p0) $ precolored strategy p0 of
    (colors0, p@(Program _ tm)) -> do
      colors <- bcolorsOfTerm strategy colors0 (annotFreeOfTerm tm)
      pure (colors, p)

bcolorsOfTerm ::
  forall e c p n a.
  Ord c =>
  Ord n =>
  ColorStrategy e c p n a ->
  Map n c ->
  Term p n (Free n a) ->
  Either (ColorError e n) (Map n c)
bcolorsOfTerm strategy colored0 = \case
  Let (Free _ _) ns0 _ tm -> do
    let
      bound =
        Set.fromList ns0

      needColor =
        Set.toList $
        bound `Set.difference` Map.keysSet colored0

      free_tm =
        freeVars $ annotOfTerm tm

      required =
        bound `Set.union` free_tm

      go :: Map n c -> n -> Either (ColorError e n) (Map n c)
      go colors n = do
        let
          used =
            Set.fromList . Map.elems $
            colors `mapIntersectionSet` required

        color <- first StrategyError $ unusedColor strategy n used
        pure $ Map.insert n color colors

    colored <- foldM go colored0 needColor
    bcolorsOfTerm strategy colored tm

  Return _ _ ->
    pure colored0

------------------------------------------------------------------------

mapIntersectionSet :: Ord k => Map k v -> Set k -> Map k v
mapIntersectionSet m =
  Map.intersection m . Map.fromSet (const ())
