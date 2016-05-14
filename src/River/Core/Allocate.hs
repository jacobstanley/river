{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module River.Core.Allocate where

import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T

import           River.Name
import           River.Core.Scope
import           River.Core.Syntax
import           River.Core.Example
import           River.Core.Rename
import           River.Core.Pretty
import           River.X64.Register


allocatedOfProgram :: Ord n => Program n a -> Program (Name Text) a
allocatedOfProgram p =
  let
    colors =
      colorsOfProgram p

    go n = do
      color <- Map.lookup n colors
      reg <- fromColor color
      pure . Name . T.toLower . T.pack $ "%" ++ show reg
  in
    fromJust $
      renameProgram go p

fromColor :: Int -> Maybe General
fromColor =
  let
    regs =
      [ RAX
      , RBX
      , RCX
      , RDX
      , RBP
      , RSP
      , RSI
      , RDI
      , R8
      , R9
      , R10
      , R11
      , R12
      , R13
      , R14
      , R15
      ]

    i2r =
      Map.fromList $ List.zip [0..] regs
  in
    flip Map.lookup i2r

-- | Find the optimal K-coloring for the variables in a program.
colorsOfProgram :: forall n a. Ord n => Program n a -> Map n Int
colorsOfProgram p =
  let
    interference :: Map n (Set n)
    interference =
      interferenceOfProgram p

    go :: n -> Map n Int -> Map n Int
    go n colors =
      case Map.lookup n interference of
        Nothing ->
          colors
        Just neighbors ->
          let
            used =
              Set.fromList . Map.elems $
              colors `mapIntersectionSet` neighbors

            lowest =
              head $ filter (not . flip Set.member used) [0..]
          in
            Map.insert n lowest colors
  in
    foldr go Map.empty $
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
