{-# LANGUAGE LambdaCase #-}
--
-- | Redundant copy removal.
--
module River.Core.Transform.Redundant (
    redundantOfProgram
  , redundantOfTerm
  ) where

import           Data.Maybe (mapMaybe)

import           River.Bifunctor
import           River.Core.Analysis.Free
import           River.Core.Syntax
import           River.Progress

redundantOfProgram :: (Ord n, MonadProgress m) => Program k p n a -> m (Program k p n a)
redundantOfProgram = \case
  Program a tm0 -> do
    Program a . fmap freeTail <$> redundantOfTerm (annotFreeOfTerm tm0)

redundantOfTerm :: (Ord n, MonadProgress m) => Term k p n (Free n a) -> m (Term k p n (Free n a))
redundantOfTerm = \case
  Return a tl ->
    pure $
      Return a tl

  If a k i t e ->
    If a k i
      <$> redundantOfTerm t
      <*> redundantOfTerm e

  Let a ns tl@(Copy _ xs0) tm -> do
    let
      xs =
        mapMaybe takeName xs0

      only_variables =
        length xs == length xs0

    if xs == ns && only_variables then
      progress =<< redundantOfTerm tm
    else
      Let a ns tl
        <$> redundantOfTerm tm

  Let a ns tl tm ->
    Let a ns tl
      <$> redundantOfTerm tm

  LetRec a bs tm ->
    LetRec a
      <$> redundantOfBindings bs
      <*> redundantOfTerm tm

redundantOfBindings ::
  Ord n =>
  MonadProgress m =>
  Bindings k p n (Free n a) ->
  m (Bindings k p n (Free n a))
redundantOfBindings = \case
  Bindings a bs ->
    Bindings a <$> traverse (secondA redundantOfBinding) bs

redundantOfBinding ::
  Ord n =>
  MonadProgress m =>
  Binding k p n (Free n a) ->
  m (Binding k p n (Free n a))
redundantOfBinding = \case
  Lambda a ns tm ->
    Lambda a ns <$> redundantOfTerm tm
