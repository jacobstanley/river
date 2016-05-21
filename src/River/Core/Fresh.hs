{-# LANGUAGE LambdaCase #-}
module River.Core.Fresh (
    nextOfProgram
  , nextOfTerm
  , nextOfTail
  , nextOfAtom
  , nextOfName
  ) where

import           Data.List (foldl')

import           River.Core.Syntax
import           River.Name

nextOfProgram :: Program p (Name n) a -> Int
nextOfProgram = \case
  Program _ tm ->
    nextOfTerm tm

nextOfTerm :: Term p (Name n) a -> Int
nextOfTerm = \case
  Let _ ns tl tm ->
    foldl' (+) 0 $
      nextOfTail tl :
      nextOfTerm tm :
      fmap nextOfName ns
  Return _ tl ->
    nextOfTail tl

nextOfTail :: Tail p (Name n) a -> Int
nextOfTail = \case
  Copy _ xs ->
    foldl' (+) 0 $
      fmap nextOfAtom xs
  Prim _ _ xs ->
    foldl' (+) 0 $
      fmap nextOfAtom xs

nextOfAtom :: Atom (Name n) a -> Int
nextOfAtom = \case
  Immediate _ _ ->
    1
  Variable _ n ->
    nextOfName n

nextOfName :: Name n -> Int
nextOfName = \case
  Name _ ->
    1
  NameMod _ n ->
    n + 1
  NameNew n ->
    n + 1
