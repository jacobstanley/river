{-# LANGUAGE LambdaCase #-}
module River.Source.Check (
    CheckError(..)
  , checkProgram
  ) where

import qualified Data.Map as Map
import           Data.Set (Set)

import           River.Source.Syntax
import           River.Source.Scope

------------------------------------------------------------------------

data CheckError a =
    UndeclaredVariable    !Identifier !(Set a)
  | UninitializedVariable !Identifier !(Set a)
  | NoReturnStatement  !a
  deriving (Eq, Ord, Read, Show)

checkProgram :: Ord a => Program a -> [CheckError a]
checkProgram p =
  checkReturn p ++
  checkDeclarations p ++
  checkInitialization p

checkReturn :: Program a -> [CheckError a]
checkReturn (Program a ss) =
  let
    isReturn = \case
      Return _ _ ->
        True
      _ ->
        False
  in
    if any isReturn ss then
      []
    else
      [NoReturnStatement a]

checkDeclarations :: Ord a => Program a -> [CheckError a]
checkDeclarations =
  let
    go (k, v) =
      UndeclaredVariable k v
  in
    map go . Map.toList . freeOfProgram

checkInitialization :: Ord a => Program a -> [CheckError a]
checkInitialization =
  let
    go (k, v) =
      UninitializedVariable k v
  in
    map go . Map.toList . uninitializedOfProgram
