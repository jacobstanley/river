{-# LANGUAGE LambdaCase #-}
--
-- | Dead binding removal.
--
module River.Core.Transform.Dead (
    deadOfProgram
  , deadOfTerm
  ) where

import qualified Data.Set as Set

import           River.Bifunctor
import           River.Core.Analysis.Scope
import           River.Core.Annotation
import           River.Core.Syntax
import           River.Progress

deadOfProgram :: (Ord n, MonadProgress m) => Program k p n a -> m (Program k p n a)
deadOfProgram = \case
  Program a tm0 -> do
    Program a . fmap freeTail <$> deadOfTerm (annotFreeOfTerm tm0)

deadOfTerm :: (Ord n, MonadProgress m) => Term k p n (Free n a) -> m (Term k p n (Free n a))
deadOfTerm = \case
  Return a tl ->
    pure $
      Return a tl

  If a k i t e ->
    If a k i
      <$> deadOfTerm t
      <*> deadOfTerm e

  Let a ns tl tm0 -> do
    tm <- deadOfTerm tm0

    let
      free =
        freeVars $ annotOfTerm tm

    if any (flip Set.member free) ns then
      pure $ Let a ns tl tm
    else
      progress tm

  LetRec a bs tm ->
    LetRec a
      <$> deadOfBindings bs
      <*> deadOfTerm tm

deadOfBindings ::
  Ord n =>
  MonadProgress m =>
  Bindings k p n (Free n a) ->
  m (Bindings k p n (Free n a))
deadOfBindings = \case
  Bindings a bs ->
    -- TODO remove dead bindings here
    Bindings a <$> traverse (secondA deadOfBinding) bs

deadOfBinding ::
  Ord n =>
  MonadProgress m =>
  Binding k p n (Free n a) ->
  m (Binding k p n (Free n a))
deadOfBinding = \case
  Lambda a ns tm ->
    Lambda a ns <$> deadOfTerm tm
