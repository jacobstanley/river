{-# LANGUAGE LambdaCase #-}

-- | Hoist the else clauses of all if expressions out to letrecs and replace
--   them with a tail call to the letrec function.
module River.Core.Transform.Else (
    elseOfProgram
  , elseOfTerm
  ) where

import           River.Bifunctor
import           River.Core.Annotation
import           River.Core.Syntax
import           River.Fresh


elseOfProgram :: (Ord n, FreshName n, MonadFresh m) => Program p n a -> m (Program p n a)
elseOfProgram = \case
  Program a tm0 -> do
    Program a <$> elseOfTerm tm0

elseOfTerm ::
  Ord n =>
  FreshName n =>
  MonadFresh m =>
  Term p n a ->
  m (Term p n a)
elseOfTerm = \case
  Return a tl ->
    pure $
      Return a tl

  If a i t e0 -> do
    e <- elseOfTerm e0
    ne <- newFresh

    let
      ae =
        annotOfTerm e

    -- NOTE: The else term will never be able to refer to anything which isn't
    -- NOTE: already in scope for the whole of the if. So there is no need to
    -- NOTE: thread through any free variables as arguments when hoisting it
    -- NOTE: out to a letrec.

    pure $
      LetRec ae
        (Bindings ae [(ne, Lambda ae [] e)]) $
      If a i t $
        Return ae (Call ae ne [])

  Let a ns tl tm ->
    Let a ns tl <$> elseOfTerm tm

  LetRec a bs tm ->
    LetRec a
      <$> elseOfBindings bs
      <*> elseOfTerm tm

elseOfBindings ::
  Ord n =>
  FreshName n =>
  MonadFresh m =>
  Bindings p n a ->
  m (Bindings p n a)
elseOfBindings = \case
  Bindings a bs0 -> do
    Bindings a <$> traverse (secondA elseOfBinding) bs0

elseOfBinding ::
  Ord n =>
  FreshName n =>
  MonadFresh m =>
  Binding p n a ->
  m (Binding p n a)
elseOfBinding = \case
  Lambda a ns tm ->
    Lambda a ns <$> elseOfTerm tm
