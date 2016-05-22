{-# LANGUAGE LambdaCase #-}
module River.Core.Fresh (
    nextOfProgram
  , nextOfTerm
  , nextOfTail
  , nextOfAtom
  ) where

import           Data.List (foldl')

import           River.Core.Syntax
import           River.Fresh


nextOfProgram :: FreshName n => Program p n a -> Int
nextOfProgram = \case
  Program _ tm ->
    nextOfTerm tm

nextOfTerm :: FreshName n => Term p n a -> Int
nextOfTerm = \case
  Let _ ns tl tm ->
    foldl' max 0 $
      nextOfTail tl :
      nextOfTerm tm :
      fmap nextName ns
  Return _ tl ->
    nextOfTail tl

nextOfTail :: FreshName n => Tail p n a -> Int
nextOfTail = \case
  Copy _ xs ->
    foldl' max 0 $
      fmap nextOfAtom xs
  Prim _ _ xs ->
    foldl' max 0 $
      fmap nextOfAtom xs

nextOfAtom :: FreshName n => Atom n a -> Int
nextOfAtom = \case
  Immediate _ _ ->
    1
  Variable _ n ->
    nextName n
