{-# LANGUAGE LambdaCase #-}

module River.Source.Scope where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import           River.Source.Syntax

------------------------------------------------------------------------

fvOfProgram :: Ord a => Program a -> Map Identifier (Set a)
fvOfProgram = \case
  Program _ ss -> fvOfStatements ss

fvOfStatements :: Ord a => [Statement a] -> Map Identifier (Set a)
fvOfStatements []     = Map.empty
fvOfStatements (s:ss) = case s of
  Declaration _ n mx
   -> let
          fx  = maybe Map.empty fvOfExpression mx
          fss = fvOfStatements ss
      in
          union fx fss `Map.difference` singleton' n

  Assignment a n _ x
   -> singleton n a `union` fvOfExpression x
                    `union` fvOfStatements ss

  Return _ x
   -> fvOfExpression x `union` fvOfStatements ss

fvOfExpression :: Ord a => Expression a -> Map Identifier (Set a)
fvOfExpression = \case
  Literal{}        -> Map.empty
  Variable a n     -> singleton n a
  Unary    _ _ x   -> fvOfExpression x
  Binary   _ _ x y -> fvOfExpression x `union` fvOfExpression y

------------------------------------------------------------------------

singleton :: k -> a -> Map k (Set a)
singleton n a = Map.singleton n (Set.singleton a)

singleton' :: k -> Map k (Set a)
singleton' n = Map.singleton n Set.empty

union :: (Ord k, Ord a) => Map k (Set a) -> Map k (Set a) -> Map k (Set a)
union xs ys = Map.unionWith Set.union xs ys
