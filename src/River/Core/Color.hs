{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module River.Core.Color (
    ColorStrategy(..)
  , colorByInt

  , coloredOfProgram
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

import           River.Core.Analysis.Bindings
import           River.Core.Analysis.Interference
import           River.Core.Analysis.Simplicial
import           River.Core.Syntax

------------------------------------------------------------------------

data ColorError e n =
    MissingFromInterference !n
  | MissingFromColorMap !n
  | MissingFromBindings !n
  | StrategyError !e
    deriving (Eq, Ord, Show)

data ColorStrategy e c n a =
  ColorStrategy {
      -- | Given the binding and the set of colors in use by neighbors, return
      --   the color to assign to the current variable.
      unusedColor :: Binding n a -> Set c -> Either e c
    }

-- | Simple coloring strategy which colors the graph using integers.
colorByInt :: ColorStrategy Void Int n a
colorByInt =
  ColorStrategy $ \_ used ->
    pure . head $ filter (not . flip Set.member used) [0..]

-- | Rename variables to their optimal K-coloring.
coloredOfProgram ::
  Ord c =>
  Ord n =>
  ColorStrategy e c n a ->
  Program n a ->
  Either (ColorError e n) (Program c a)
coloredOfProgram strategy p = do
  let
    lookupName colors n =
      case Map.lookup n colors of
        Nothing ->
          Left $ MissingFromColorMap n
        Just x ->
          Right x
  colors <- colorsOfProgram strategy p
  bitraverse (lookupName colors) pure p

-- | Find the optimal K-coloring for the variables in a program.
colorsOfProgram ::
  forall e c n a.
  Ord c =>
  Ord n =>
  ColorStrategy e c n a ->
  Program n a ->
  Either (ColorError e n) (Map n c)
colorsOfProgram strategy p =
  let
    interference :: InterferenceGraph n
    interference =
      interferenceOfProgram p

    bindings :: Map n (Binding n a)
    bindings =
      bindingsOfProgram p

    go :: Map n c -> n -> Either (ColorError e n) (Map n c)
    go colors n = do
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
    foldM go Map.empty $
    simplicial interference

mapIntersectionSet :: Ord k => Map k v -> Set k -> Map k v
mapIntersectionSet m =
  Map.intersection m . Map.fromSet (const ())
