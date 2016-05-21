{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module River.Core.Color (
    ColorStrategy(..)
  , colorByInt

  , coloredOfProgram

  , scolorsOfProgram

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

import           River.Core.Analysis.Bindings
import           River.Core.Analysis.Interference
import           River.Core.Analysis.Simplicial
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
      unusedColor :: Binding p n a -> Set c -> Either e c

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
      simplicial interference

    bindings :: Map n (Binding p n a)
    bindings =
      bindingsOfProgram p

    go :: Map n c -> n -> Either (ColorError e n) (Map n c)
    go colors n =
      if Map.member n colors then
        -- pre-colored node
        pure colors
      else do
        ns <-
          maybe (Left $ MissingFromInterference n) pure $
          neighbors n interference

        binding <-
          maybe (Left $ MissingFromBindings n) pure $
          Map.lookup n bindings

        let
          used =
            Set.fromList . Map.elems $
            colors `mapIntersectionSet` ns

        color <-
          first StrategyError $
          unusedColor strategy binding used

        pure $
          Map.insert n color colors
  in
    foldM go (precolored strategy p) ordering

mapIntersectionSet :: Ord k => Map k v -> Set k -> Map k v
mapIntersectionSet m =
  Map.intersection m . Map.fromSet (const ())
