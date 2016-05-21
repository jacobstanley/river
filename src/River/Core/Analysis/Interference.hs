{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module River.Core.Analysis.Interference (
    InterferenceGraph
  , mapOfInterference
  , neighbors
  , fromNeighboring

  , interferenceOfProgram
  , interferenceOfTerm
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid ((<>))
import           Data.Set (Set)
import qualified Data.Set as Set

import           River.Core.Scope
import           River.Core.Syntax


newtype InterferenceGraph n =
  InterferenceGraph (Map n (Set n))
  deriving (Eq, Ord, Show)

instance Ord n => Monoid (InterferenceGraph n) where
  mempty =
    InterferenceGraph Map.empty

  mappend (InterferenceGraph xs) (InterferenceGraph ys) =
    InterferenceGraph $
      Map.unionWith Set.union xs ys

mapOfInterference :: InterferenceGraph n -> Map n (Set n)
mapOfInterference (InterferenceGraph xs) =
  xs

neighbors :: Ord n => n -> InterferenceGraph n -> Maybe (Set n)
neighbors n =
  Map.lookup n . mapOfInterference

fromBinding :: Ord n => n -> InterferenceGraph n
fromBinding n =
  InterferenceGraph $ Map.singleton n Set.empty

fromNeighboring :: forall n. Ord n => Set n -> InterferenceGraph n
fromNeighboring =
  let
    go :: [n] -> [n] -> [(n, Set n)]
    go xs = \case
     [] ->
       []
     y : ys ->
       (y, Set.fromList (xs ++ ys)) : go (y : xs) ys
  in
    InterferenceGraph . Map.fromList . go [] . Set.toList

-- | Find the interference graph of a program.
interferenceOfProgram :: Ord n => Program n a -> InterferenceGraph n
interferenceOfProgram = \case
  Program _ tm ->
    interferenceOfTerm tm

interferenceOfTerm :: Ord n => Term n a -> InterferenceGraph n
interferenceOfTerm xx =
  let
    interference ns =
      --
      -- Overly conservative:
      --   fromNeighboring (Set.fromList ns <> freeOfTerm xx)
      --
      mconcat $
        fromNeighboring (freeOfTerm xx) : fmap fromBinding ns
  in
    case xx of
      Let _ ns _ tm ->
        interference ns <> interferenceOfTerm tm
      Return _ _ ->
        interference []
