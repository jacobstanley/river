{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module River.Core.Color (
    ColorStrategy(..)
  , colorByInt

  , coloredOfProgram

  , scolorsOfProgram
  , colorsOfProgram

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
import           River.Core.Scope
import           River.Core.Syntax

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
    , precolored :: Program p n a -> Map n c
    }

-- | Simple coloring strategy which colors the graph using integers.
colorByInt :: ColorStrategy Void Int p n a
colorByInt =
  ColorStrategy {
      unusedColor =
        \_ used ->
          pure . head $ filter (not . flip Set.member used) [0..]
    , precolored =
        \_ ->
          Map.empty
    }

-- | Rename variables to their optimal K-coloring.
coloredOfProgram ::
  Ord c =>
  Ord n =>
  ColorStrategy e c p n a ->
  Program p n a ->
  Either (ColorError e n) (Program p c a)
coloredOfProgram strategy p = do
  let
    lookupName colors n =
      case Map.lookup n colors of
        Nothing ->
          Left $ MissingFromColorMap n
        Just x ->
          Right x
  colors <- scolorsOfProgram strategy p
  bitraverse (lookupName colors) pure p

------------------------------------------------------------------------

-- | Find the optimal K-coloring for the variables in a program.
--
--   Simplical elimination ordering method.
--
scolorsOfProgram ::
  forall e c p n a.
  Ord c =>
  Ord n =>
  ColorStrategy e c p n a ->
  Program p n a ->
  Either (ColorError e n) (Map n c)
scolorsOfProgram strategy p =
  let
    interference :: InterferenceGraph n
    interference =
      interferenceOfProgram p

    ordering :: [n]
    ordering =
      -- simplicial interference
      orderingOfProgram p

    go :: Map n c -> n -> Either (ColorError e n) (Map n c)
    go colors n =
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
  in
    foldM go (precolored strategy p) ordering

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
colorsOfProgram ::
  Ord c =>
  Ord n =>
  ColorStrategy e c p n a ->
  Program p n a ->
  Either (ColorError e n) (Map n c)
colorsOfProgram strategy p =
  case p of
    Program _ tm ->
      colorsOfTerm strategy (precolored strategy p) (annotFreeOfTerm tm)

colorsOfTerm ::
  forall e c p n a.
  Ord c =>
  Ord n =>
  ColorStrategy e c p n a ->
  Map n c ->
  Term p n (Free n a) ->
  Either (ColorError e n) (Map n c)
colorsOfTerm strategy colored0 = \case
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
    colorsOfTerm strategy colored tm

  Return _ _ ->
    pure colored0

------------------------------------------------------------------------

mapIntersectionSet :: Ord k => Map k v -> Set k -> Map k v
mapIntersectionSet m =
  Map.intersection m . Map.fromSet (const ())
