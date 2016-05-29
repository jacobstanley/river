{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module River.Core.Transform.Rename (
    renameProgram
  , renameTerm
  ) where

import           Control.Monad.Trans.State.Strict (StateT, evalStateT, get, put)

import           Data.Either (lefts)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import           River.Core.Syntax
import           River.Fresh


renameProgram :: (Ord n, FreshName n, MonadFresh m) => Program k p n a -> m (Program k p n a)
renameProgram = \case
  Program a tm ->
    flip evalStateT Set.empty $
      Program a <$> renameTerm Map.empty tm

renameTerm :: (Ord n, FreshName n, MonadFresh m) => Map n n -> Term k p n a -> StateT (Set n) m (Term k p n a)
renameTerm subs0 = \case
  Return a tl ->
    pure . Return a $ renameTail subs0 tl

  If a k i0 t0 e0 -> do
    let
      i = renameAtom subs0 i0

    t <- renameTerm subs0 t0
    e <- renameTerm subs0 e0

    pure $
      If a k i t e

  Let a ns0 tl0 tm -> do
    let
      tl =
        renameTail subs0 tl0
    (subs, ns) <- renameNames subs0 ns0
    Let a ns tl <$> renameTerm subs tm

  LetRec a bs0 tm -> do
    (subs, bs) <- renameBindings subs0 bs0
    LetRec a bs <$> renameTerm subs tm

renameBindings :: (Ord n, FreshName n, MonadFresh m) => Map n n -> Bindings k p n a -> StateT (Set n) m (Map n n, Bindings k p n a)
renameBindings subs0 = \case
  Bindings a nbs0 -> do
    let
      (ns0, bs0) =
        unzip nbs0

    (subs, ns) <- renameNames subs0 ns0
    bs <- traverse (renameBinding subs) bs0

    let
      nbs =
        zip ns bs

    pure (subs, Bindings a nbs)

renameBinding :: (Ord n, FreshName n, MonadFresh m) => Map n n -> Binding k p n a -> StateT (Set n) m (Binding k p n a)
renameBinding subs0 = \case
  Lambda a ns0 tm -> do
    (subs, ns) <- renameNames subs0 ns0
    Lambda a ns <$> renameTerm subs tm

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

renameNames :: (Ord n, FreshName n, MonadFresh m) => Map n n -> [n] -> StateT (Set n) m (Map n n, [n])
renameNames subs0 ns0 = do
    used0 <- get
    conflicts <- traverse (freshenConflict used0) ns0

    let
      ns =
        fmap (either snd id) conflicts

      subs1 =
        Map.fromList $ lefts conflicts

      subs =
        subs1 `Map.union` subs0

      used =
        Set.fromList ns `Set.union` used0

    put used
    pure (subs, ns)

freshenConflict :: (Ord n, FreshName n, MonadFresh m) => Set n -> n -> m (Either (n, n) n)
freshenConflict used n0 =
  if Set.member n0 used then do
    n <- freshen n0
    pure $ Left (n0, n)
  else
    pure $ Right n0
