{-# LANGUAGE LambdaCase #-}
module River.Source.Analysis.Live (
    liveOfProgram
  , liveOfBlock
  , liveOfStatement
  , liveOfExpression
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import           River.Map
import           River.Source.Analysis.Defined
import           River.Source.Syntax


liveOfProgram :: Ord a => Program a -> Map Identifier (Set a)
liveOfProgram = \case
  Program _ ss ->
    liveOfBlock Set.empty ss

liveOfBlock :: Ord a => Set Identifier -> Block a -> Map Identifier (Set a)
liveOfBlock declared = \case
  Block _ [] ->
    Map.empty

  Block a (s : ss) ->
    let
      here =
        liveOfStatement declared s

      there =
        liveOfBlock declared (Block a ss) `mapDifferenceSet`
        definedOfStatement declared s
    in
      here `mapSetUnion` there

liveOfStatement :: Ord a => Set Identifier -> Statement a -> Map Identifier (Set a)
liveOfStatement declared = \case
  Declare _ _ n ss ->
    Map.delete n $
    liveOfBlock (Set.insert n declared) ss

  Assign _ _ x ->
    liveOfExpression x

  If _ i t e ->
    liveOfExpression i `mapSetUnion`
    liveOfBlock declared t `mapSetUnion`
    liveOfBlock declared e

  Return _ x ->
    liveOfExpression x

liveOfExpression :: Ord a => Expression a -> Map Identifier (Set a)
liveOfExpression = \case
  Literal _ _ ->
    Map.empty
  Variable a n ->
    mapSetSingleton n a
  Unary _ _ x ->
    liveOfExpression x
  Binary _ _ x y ->
    liveOfExpression x `mapSetUnion`
    liveOfExpression y
  Conditional _ i t e ->
    liveOfExpression i `mapSetUnion`
    liveOfExpression t `mapSetUnion`
    liveOfExpression e
