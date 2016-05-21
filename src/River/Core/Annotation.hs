{-# LANGUAGE LambdaCase #-}
module River.Core.Annotation (
    annotOfProgram
  , annotOfTerm
  , annotOfTail
  , annotOfAtom
  ) where

import           River.Core.Syntax


annotOfProgram :: Program p n a -> a
annotOfProgram = \case
  Program a _ ->
    a

annotOfTerm :: Term p n a -> a
annotOfTerm = \case
  Let a _ _ _ ->
    a
  Return a _ ->
    a

annotOfTail :: Tail p n a -> a
annotOfTail = \case
  Copy a _ ->
    a
  Prim a _ _ ->
    a

annotOfAtom :: Atom n a -> a
annotOfAtom = \case
  Immediate a _ ->
    a
  Variable a _ ->
    a
