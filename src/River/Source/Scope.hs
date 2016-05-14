{-# LANGUAGE LambdaCase #-}
module River.Source.Scope (
    freeOfProgram
  , freeOfStatements
  , freeOfExpression

  , uninitializedOfProgram
  , uninitializedOfStatements
  , uninitializedOfExpression
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import           River.Source.Syntax

------------------------------------------------------------------------
-- Free Variables

freeOfProgram :: Ord a => Program a -> Map Identifier (Set a)
freeOfProgram = \case
  Program _ ss ->
    freeOfStatements ss

freeOfStatements :: Ord a => [Statement a] -> Map Identifier (Set a)
freeOfStatements []     = Map.empty
freeOfStatements (s:ss) = case s of
  Declaration _ n mx ->
    let
      fx =
        maybe Map.empty freeOfExpression mx

      fss =
        freeOfStatements ss
    in
      union fx fss `Map.difference`
      singleton' n

  Assignment a n _ x ->
    singleton n a `union`
    freeOfExpression x `union`
    freeOfStatements ss

  Return _ x ->
    freeOfExpression x `union`
    freeOfStatements ss

freeOfExpression :: Ord a => Expression a -> Map Identifier (Set a)
freeOfExpression = \case
  Literal _ _ ->
    Map.empty
  Variable a n ->
    singleton n a
  Unary _ _ x ->
    freeOfExpression x
  Binary _ _ x y ->
    freeOfExpression x `union`
    freeOfExpression y

------------------------------------------------------------------------
-- Uninitialized Variables

uninitializedOfProgram :: Ord a => Program a -> Map Identifier (Set a)
uninitializedOfProgram = \case
  Program _ ss ->
    uninitializedOfStatements ss

uninitializedOfStatements :: Ord a => [Statement a] -> Map Identifier (Set a)
uninitializedOfStatements = \case
  [] ->
    Map.empty

  Declaration _ _ Nothing : ss ->
    uninitializedOfStatements ss

  Declaration _ n (Just x) : ss ->
   let
     ux =
       uninitializedOfExpression x

     uss =
       uninitializedOfStatements ss
   in
     union ux (uss `Map.difference` singleton' n)

  Assignment _ n _ x : ss ->
    let
      ux =
        uninitializedOfExpression x

      uss =
        uninitializedOfStatements ss
    in
      union ux (uss `Map.difference` singleton' n)

  Return _ x : ss ->
    uninitializedOfExpression x `union`
    uninitializedOfStatements ss

uninitializedOfExpression :: Ord a => Expression a -> Map Identifier (Set a)
uninitializedOfExpression = \case
  Literal _ _ ->
    Map.empty
  Variable a n ->
    singleton n a
  Unary _ _ x ->
    uninitializedOfExpression x
  Binary _ _ x y ->
    uninitializedOfExpression x `union`
    uninitializedOfExpression y

------------------------------------------------------------------------

singleton :: k -> a -> Map k (Set a)
singleton n a =
  Map.singleton n (Set.singleton a)

singleton' :: k -> Map k (Set a)
singleton' n =
  Map.singleton n Set.empty

union :: (Ord k, Ord a) => Map k (Set a) -> Map k (Set a) -> Map k (Set a)
union xs ys =
  Map.unionWith Set.union xs ys
