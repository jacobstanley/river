{-# LANGUAGE LambdaCase #-}
module River.Core.Scope (
    freeOfProgram
  , freeOfTerm
  , freeOfTail
  , freeOfAtom
  ) where

import           Data.Set (Set)
import qualified Data.Set as Set

import           River.Core.Syntax


freeOfProgram :: Ord n => Program p n a -> Set n
freeOfProgram = \case
  Program _ tm ->
    freeOfTerm tm

freeOfTerm :: Ord n => Term p n a -> Set n
freeOfTerm = \case
  Let _ ns tl tm ->
    Set.union
      (freeOfTail tl)
      (freeOfTerm tm `Set.difference` Set.fromList ns)
  Return _ tl ->
    freeOfTail tl

freeOfTail :: Ord n => Tail p n a -> Set n
freeOfTail = \case
  Copy _ xs ->
    Set.unions $ fmap freeOfAtom xs
  Prim _ _ xs ->
    Set.unions $ fmap freeOfAtom xs

freeOfAtom :: Atom n a -> Set n
freeOfAtom = \case
  Immediate _ _ ->
    Set.empty
  Variable _ n ->
    Set.singleton n
