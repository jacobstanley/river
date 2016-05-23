{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module River.Core.Transform.Rename (
    renameProgram
  , renameTerm
  ) where

import           Data.Either (lefts)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import           River.Core.Syntax
import           River.Fresh


renameProgram :: (Ord n, FreshName n) => Program p n a -> Fresh (Program p n a)
renameProgram = \case
  Program a tm ->
    Program a <$> renameTerm Set.empty Map.empty tm

renameTerm :: (Ord n, FreshName n) => Set n -> Map n n -> Term p n a -> Fresh (Term p n a)
renameTerm used0 subs0 = \case
  Return a tl ->
    pure . Return a $ renameTail subs0 tl

  If a i0 t0 e0 -> do
    let
      i = renameAtom subs0 i0

    t <- renameTerm used0 subs0 t0
    e <- renameTerm used0 subs0 e0

    pure $
      If a i t e

  Let a ns0 tl0 tm -> do
    let
      freshenConflict n0 =
        if Set.member n0 used0 then do
          n <- freshen n0
          pure $ Left (n0, n)
        else
          pure $ Right n0

    conflicts <- traverse freshenConflict ns0

    let
      ns =
        fmap (either snd id) conflicts

      tl =
        renameTail subs0 tl0

      subs1 =
        Map.fromList $ lefts conflicts

      subs =
        subs1 `Map.union` subs0

      used =
        Set.fromList ns `Set.union` used0

    Let a ns tl <$> renameTerm used subs tm

  LetRec a bs tm ->
    -- TODO fucked
    LetRec a bs <$> renameTerm used0 subs0 tm

renameTail :: Ord n => Map n n -> Tail p n a -> Tail p n a
renameTail subs = \case
  Copy a xs ->
    Copy a $ fmap (renameAtom subs) xs
  Call a n xs ->
    Call a n $ fmap (renameAtom subs) xs
  Prim a p xs ->
    Prim a p $ fmap (renameAtom subs) xs

renameAtom :: Ord n => Map n n -> Atom n a -> Atom n a
renameAtom subs = \case
  Immediate a i ->
    Immediate a i
  Variable a n0 ->
    case Map.lookup n0 subs of
      Nothing ->
        Variable a n0
      Just n ->
        Variable a n
