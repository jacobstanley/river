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


nextOfProgram :: FreshName n => Program k p n a -> Int
nextOfProgram = \case
  Program _ tm ->
    nextOfTerm tm

nextOfTerm :: FreshName n => Term k p n a -> Int
nextOfTerm = \case
  Return _ tl ->
    nextOfTail tl
  If _ _ i t e ->
    nextOfAtom i `max`
    nextOfTerm t `max`
    nextOfTerm e
  Let _ ns tl tm ->
    foldl' max 0 $
      nextOfTail tl :
      nextOfTerm tm :
      fmap nextName ns
  LetRec _ bs tm ->
    nextOfBindings bs `max`
    nextOfTerm tm

nextOfBindings :: FreshName n => Bindings k p n a -> Int
nextOfBindings = \case
  Bindings _ bs ->
    let
      go acc (n, b) =
        acc `max`
        nextName n `max`
        nextOfBinding b
    in
      foldl' go 0 bs

nextOfBinding :: FreshName n => Binding k p n a -> Int
nextOfBinding = \case
  Lambda _ ns tm ->
    foldl' max 0 $
      nextOfTerm tm :
      fmap nextName ns

nextOfTail :: FreshName n => Tail p n a -> Int
nextOfTail = \case
  Copy _ xs ->
    foldl' max 0 $
      fmap nextOfAtom xs
  Call _ _ xs ->
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
