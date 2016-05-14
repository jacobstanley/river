{-# LANGUAGE LambdaCase #-}
module River.Core.Rename (
    renameProgram
  , renameTerm
  , renameTail
  , renameAtom
  ) where

import           River.Core.Syntax


renameProgram :: Applicative f => (n -> f x) -> Program n a -> f (Program x a)
renameProgram f = \case
  Program a tm ->
    Program a <$> renameTerm f tm

renameTerm :: Applicative f => (n -> f x) -> Term n a -> f (Term x a)
renameTerm f = \case
  Let a ns tl tm ->
    Let a <$> traverse f ns <*> renameTail f tl <*> renameTerm f tm
  Return a tl ->
    Return a <$> renameTail f tl

renameTail :: Applicative f => (n -> f x) -> Tail n a -> f (Tail x a)
renameTail f = \case
  Copy a xs ->
    Copy a <$> traverse (renameAtom f) xs
  Unary a op x ->
    Unary a op <$> renameAtom f x
  Binary a op x y ->
    Binary a op <$> renameAtom f x <*> renameAtom f y

renameAtom :: Applicative f => (n -> f x) -> Atom n a -> f (Atom x a)
renameAtom f = \case
  Immediate a i ->
    pure $ Immediate a i
  Variable a n ->
    Variable a <$> f n
