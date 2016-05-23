{-# LANGUAGE LambdaCase #-}
module River.Core.Annotation (
    annotOfProgram
  , annotOfTerm
  , annotOfTail
  , annotOfAtom
  , annotOfBindings
  , annotOfBinding
  ) where

import           River.Core.Syntax


annotOfProgram :: Program p n a -> a
annotOfProgram = \case
  Program a _ ->
    a

annotOfTerm :: Term p n a -> a
annotOfTerm = \case
  Return a _ ->
    a
  If a _ _ _ ->
    a
  Let a _ _ _ ->
    a
  LetRec a _ _ ->
    a

annotOfTail :: Tail p n a -> a
annotOfTail = \case
  Copy a _ ->
    a
  Call a _ _ ->
    a
  Prim a _ _ ->
    a

annotOfAtom :: Atom n a -> a
annotOfAtom = \case
  Immediate a _ ->
    a
  Variable a _ ->
    a

annotOfBindings :: Bindings p n a -> a
annotOfBindings = \case
  Bindings a _ ->
    a

annotOfBinding :: Binding p n a -> a
annotOfBinding = \case
  Lambda a _ _ ->
    a
