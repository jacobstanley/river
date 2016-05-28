{-# LANGUAGE LambdaCase #-}
module River.Source.Check (
    CheckError(..)
  , checkProgram
  ) where

import qualified Data.Map.Strict as Map
import           Data.Set (Set)

import           River.Source.Analysis.Return
import           River.Source.Analysis.Scope
import           River.Source.Annotation
import           River.Source.Syntax

------------------------------------------------------------------------

data CheckError a =
    UndeclaredVariable !Identifier !a
  | UndefinedVariable !Identifier !a !(Set a)
  | NoReturnStatement !a
    deriving (Eq, Ord, Read, Show)

checkProgram :: Ord a => Program a -> [CheckError a]
checkProgram p =
  let
    checkedReturn =
      case returnOfProgram p of
        ReturnOK ->
          []
        ReturnKO a ->
          [NoReturnStatement a]

    checked =
      case scopedProgram p of
        Program _ b ->
          checkBlock b
  in
    checkedReturn ++
    checked

checkBlock :: Ord a => Block (Scope a) -> [CheckError a]
checkBlock = \case
  Block _ ss ->
    concatMap checkStatement ss

checkStatement :: Ord a => Statement (Scope a) -> [CheckError a]
checkStatement = \case
  Declare a _ n b ->
    case Map.lookup n (scopeLive $ annotOfBlock b) of
      Nothing ->
        checkBlock b
      Just uses ->
        UndefinedVariable n (scopeTail a) uses : checkBlock b

  Assign a n x ->
    if Map.member n (scopeDeclared a) then
      checkExpression x
    else
      UndeclaredVariable n (scopeTail a) : checkExpression x

  If _ i t e ->
    checkExpression i ++
    checkBlock t ++
    checkBlock e

  While _ x b ->
    checkExpression x ++
    checkBlock b

  Return _ x ->
    checkExpression x

checkExpression :: Ord a => Expression (Scope a) -> [CheckError a]
checkExpression = \case
  Literal _ _ ->
    []

  Variable a n ->
    if Map.member n (scopeDeclared a) then
      []
    else
      [UndeclaredVariable n (scopeTail a)]

  Unary _ _ x ->
    checkExpression x

  Binary _ _ x y ->
    checkExpression x ++
    checkExpression y

  Conditional _ i t e ->
    checkExpression i ++
    checkExpression t ++
    checkExpression e
