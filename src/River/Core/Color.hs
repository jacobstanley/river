{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module River.Core.Color (
    coloredOfProgram
  , colorsOfProgram
  ) where

import           Control.Monad (foldM)

import           Data.Bitraversable (bitraverse)
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import           River.Core.Scope
import           River.Core.Syntax

------------------------------------------------------------------------

data ColorError n =
    MissingFromInterference !n
  | MissingFromColorMap !n
    deriving (Eq, Ord, Show)

-- | Rename variables to their optimal K-coloring.
coloredOfProgram :: Ord n => Program n a -> Either (ColorError n) (Program Int a)
coloredOfProgram p = do
  let
    lookupName colors n =
      case Map.lookup n colors of
        Nothing ->
          Left $ MissingFromColorMap n
        Just x ->
          Right x
  colors <- colorsOfProgram p
  bitraverse (lookupName colors) pure p

-- | Find the optimal K-coloring for the variables in a program.
colorsOfProgram :: forall n a. Ord n => Program n a -> Either (ColorError n) (Map n Int)
colorsOfProgram p =
  let
    interference :: Map n (Set n)
    interference =
      interferenceOfProgram p

    go :: Map n Int -> n -> Either (ColorError n) (Map n Int)
    go colors n =
      case Map.lookup n interference of
        Nothing ->
          Left $ MissingFromInterference n
        Just neighbors ->
          let
            used =
              Set.fromList . Map.elems $
              colors `mapIntersectionSet` neighbors

            lowest =
              head $ filter (not . flip Set.member used) [0..]
          in
            Right $ Map.insert n lowest colors
  in
    foldM go Map.empty $
    simplicial interference

mapIntersectionSet :: Ord k => Map k v -> Set k -> Map k v
mapIntersectionSet m =
  Map.intersection m . Map.fromSet (const ())

------------------------------------------------------------------------

-- | Find the interference graph of a program.
interferenceOfProgram :: Ord n => Program n a -> Map n (Set n)
interferenceOfProgram = \case
  Program _ tm ->
    interferenceOfTerm tm

interferenceOfTerm :: Ord n => Term n a -> Map n (Set n)
interferenceOfTerm xx =
  let
    interference =
      mkInterference (freeOfTerm xx)
  in
    case xx of
      Let _ _ _ tm ->
        Map.unionWith Set.union interference (interferenceOfTerm tm)
      Return _ _ ->
        interference

mkInterference :: forall n. Ord n => Set n -> Map n (Set n)
mkInterference =
  let
    go :: [n] -> [n] -> [(n, Set n)]
    go xs = \case
     [] ->
       []
     y : ys ->
       (y, Set.fromList (xs ++ ys)) : go (y : xs) ys
  in
    Map.fromList . go [] . Set.toList

------------------------------------------------------------------------

-- | Find the simplicial elimination ordering given an interference graph.
simplicial :: forall n. Ord n => Map n (Set n) -> [n]
simplicial =
  let
    go :: Map n (Int, Set n) -> [n]
    go weights0 =
      let
        weight (_, (x, _)) (_, (y, _)) =
          compare x y

        (n, (_, neighbors)) =
          List.maximumBy weight $
          Map.toList weights0

        increment =
          Map.fromSet (const (1, Set.empty)) neighbors

        append (i, s) (j, _) =
          (i + j, s)

        weights =
          Map.map (fmap $ Set.delete n) .
          Map.delete n $
          Map.unionWith append weights0 increment
      in
        if Map.null weights0 then
          []
        else
          n : go weights
  in
    go . Map.map (\neighbors -> (0, neighbors))
