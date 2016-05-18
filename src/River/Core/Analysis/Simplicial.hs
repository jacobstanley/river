{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module River.Core.Analysis.Simplicial (
    simplicial
  ) where

import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import           River.Core.Analysis.Interference (InterferenceGraph)
import           River.Core.Analysis.Interference (mapOfInterference)


-- | Find the simplicial elimination ordering given an interference graph.
simplicial :: forall n. Ord n => InterferenceGraph n -> [n]
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
    go . Map.map (\neighbors -> (0, neighbors)) . mapOfInterference
