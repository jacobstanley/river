{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
--
-- | Convert from boolean conditionals to x86-64 condition codes.
--
module River.X64.Transform.Recondition where

import           River.Bifunctor
import           River.Core.Primitive
import           River.Core.Syntax
import           River.X64.Primitive (Cc(..))


reconditionProgram :: Program () Prim n a -> Program Cc Prim n a
reconditionProgram = \case
  Program a tm ->
    Program a $ reconditionTerm tm

reconditionTerm :: Term () Prim n a -> Term Cc Prim n a
reconditionTerm = \case
  Return a tl ->
    Return a tl

  If a () i t e ->
    If a Nz i
      (reconditionTerm t)
      (reconditionTerm e)

  Let a ns tl tm ->
    Let a ns tl
      (reconditionTerm tm)

  LetRec a bs tm ->
    LetRec a
      (reconditionBindings bs)
      (reconditionTerm tm)


reconditionBindings :: Bindings () Prim n a -> Bindings Cc Prim n a
reconditionBindings = \case
  Bindings a bs ->
    Bindings a $
      fmap (second reconditionBinding) bs

reconditionBinding :: Binding () Prim n a -> Binding Cc Prim n a
reconditionBinding = \case
  Lambda a ns tm ->
    Lambda a ns
      (reconditionTerm tm)
