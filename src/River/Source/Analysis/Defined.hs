{-# LANGUAGE LambdaCase #-}
module River.Source.Analysis.Defined (
    definedOfProgram
  , definedOfBlock
  , definedOfStatement
  ) where

import           Data.Set (Set)
import qualified Data.Set as Set

import           River.Source.Syntax


definedOfProgram :: Program a -> Set Identifier
definedOfProgram = \case
  Program _ ss ->
    definedOfBlock Set.empty ss

definedOfBlock :: Set Identifier -> Block a -> Set Identifier
definedOfBlock declared = \case
  Block _ [] ->
    Set.empty
  Block a (s : ss) ->
    definedOfStatement declared s `Set.union`
    definedOfBlock declared (Block a ss)

definedOfStatement :: Set Identifier -> Statement a -> Set Identifier
definedOfStatement declared = \case
  Declare _ _ n ss ->
    Set.delete n $
    definedOfBlock (Set.insert n declared) ss

  Assign _ n _ ->
    Set.singleton n

  If _ _ t e ->
    definedOfBlock declared t `Set.intersection`
    definedOfBlock declared e

  -- Returning defines all x in scope, because it transfers control out of the
  -- scope.
  Return _ _ ->
    declared
