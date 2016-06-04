{-# LANGUAGE LambdaCase #-}
--
-- | Hoist the then clauses of all if expressions out to letrecs and replace
--   them with a tail call to the letrec function.
--
--   This turns if expressions in to conditional jumps.
--
module River.Core.Transform.Jump (
    jumpOfProgram
  , jumpOfTerm
  ) where

import           River.Bifunctor
import           River.Core.Annotation
import           River.Core.Syntax
import           River.Fresh


jumpOfProgram :: (Ord n, FreshName n, MonadFresh m) => Program k p n a -> m (Program k p n a)
jumpOfProgram = \case
  Program a tm0 -> do
    Program a <$> jumpOfTerm tm0

jumpOfTerm ::
  Ord n =>
  FreshName n =>
  MonadFresh m =>
  Term k p n a ->
  m (Term k p n a)
jumpOfTerm = \case
  Return a tl ->
    pure $
      Return a tl

  If a k i t0 e -> do
    t <- jumpOfTerm t0
    nt <- newFresh

    let
      at =
        annotOfTerm t

    -- NOTE: The then term will never be able to refer to anything which isn't
    -- NOTE: already in scope for the whole of the if. So there is no need to
    -- NOTE: thread through any free variables as arguments when hoisting it
    -- NOTE: out to a letrec.

    pure $
      LetRec at
        (Bindings at [(nt, Lambda at [] t)]) $
      If a k i
        (Return at (Call at nt []))
        e

  Let a ns tl tm ->
    Let a ns tl <$> jumpOfTerm tm

  LetRec a bs tm ->
    LetRec a
      <$> jumpOfBindings bs
      <*> jumpOfTerm tm

jumpOfBindings ::
  Ord n =>
  FreshName n =>
  MonadFresh m =>
  Bindings k p n a ->
  m (Bindings k p n a)
jumpOfBindings = \case
  Bindings a bs0 -> do
    Bindings a <$> traverse (secondA jumpOfBinding) bs0

jumpOfBinding ::
  Ord n =>
  FreshName n =>
  MonadFresh m =>
  Binding k p n a ->
  m (Binding k p n a)
jumpOfBinding = \case
  Lambda a ns tm ->
    Lambda a ns <$> jumpOfTerm tm
