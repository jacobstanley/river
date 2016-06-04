{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
--
-- | Flip then/else branches of if expressions.
--
module River.X64.Transform.Flip (
    flipCc

  , flipProgram
  , flipTerm
  ) where

import           River.Bifunctor
import           River.Core.Syntax
import           River.X64.Primitive


flipProgram :: Program Cc Prim n a -> Program Cc Prim n a
flipProgram = \case
  Program a tm ->
    Program a $ flipTerm tm

flipTerm :: Term Cc Prim n a -> Term Cc Prim n a
flipTerm = \case
  Return a tl ->
    Return a tl

  If a cc i t e ->
    If a (flipCc cc) i (flipTerm e) (flipTerm t)

  Let a ns tl tm ->
    Let a ns tl (flipTerm tm)

  LetRec a bs tm ->
    LetRec a (flipBindings bs) (flipTerm tm)

flipBindings :: Bindings Cc Prim n a -> Bindings Cc Prim n a
flipBindings = \case
  Bindings a bs ->
    Bindings a
      (fmap (second flipBinding) bs)

flipBinding :: Binding Cc Prim n a -> Binding Cc Prim n a
flipBinding = \case
  Lambda a ns tm ->
    Lambda a ns
      (flipTerm tm)

flipCc :: Cc -> Cc
flipCc = \case
  Z ->
    Nz
  E ->
    Ne
  Nz ->
    Z
  Ne ->
    E
  L ->
    Ge
  Le ->
    G
  G ->
    Le
  Ge ->
    L
