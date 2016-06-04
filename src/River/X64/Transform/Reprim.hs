{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
--
-- | Convert from core primitives to x86-64 primitives.
--
module River.X64.Transform.Reprim (
    reprimProgram
  , reprimTerm

  , ReprimError(..)
  ) where

import           Control.Monad.Trans.Except (ExceptT, throwE)

import           River.Bifunctor
import qualified River.Core.Primitive as Core
import           River.Core.Syntax
import           River.Fresh
import           River.X64.Primitive (Cc(..))
import qualified River.X64.Primitive as X64


data ReprimError n a =
    ReprimInvalidPrim ![n] Core.Prim ![Atom n a]
    deriving (Eq, Ord, Show, Functor)

reprimProgram ::
  FreshName n =>
  MonadFresh m =>
  Program k Core.Prim n a ->
  ExceptT (ReprimError n a) m (Program k X64.Prim n a)
reprimProgram = \case
  Program a tm ->
    Program a <$> reprimTerm tm

reprimTerm ::
  FreshName n =>
  MonadFresh m =>
  Term k Core.Prim n a ->
  ExceptT (ReprimError n a) m (Term k X64.Prim n a)
reprimTerm = \case
  Return ar (Call ac n xs) ->
    pure $
      Return ar (Call ac n xs)

  Return a tl -> do
    -- TODO should be based on arity of tail, not just [n]
    -- TODO need to do arity inference before this is possible.
    n <- newFresh
    let_tail <- reprimTail a [n] tl
    pure . let_tail $
      Return a (Copy a [Variable a n])

  If a k i t e -> do
    If a k i
      <$> reprimTerm t
      <*> reprimTerm e

  Let a ns tl tm -> do
    let_tail <- reprimTail a ns tl
    let_tail <$> reprimTerm tm

  LetRec a bs tm ->
    LetRec a
      <$> reprimBindings bs
      <*> reprimTerm tm

reprimBindings ::
  FreshName n =>
  MonadFresh m =>
  Bindings k Core.Prim n a ->
  ExceptT (ReprimError n a) m (Bindings k X64.Prim n a)
reprimBindings = \case
  Bindings a bs ->
    Bindings a <$> traverse (secondA reprimBinding) bs

reprimBinding ::
  FreshName n =>
  MonadFresh m =>
  Binding k Core.Prim n a ->
  ExceptT (ReprimError n a) m (Binding k X64.Prim n a)
reprimBinding = \case
  Lambda a ns tm ->
    Lambda a ns <$> reprimTerm tm

reprimTail ::
  FreshName n =>
  MonadFresh m =>
  a ->
  [n] ->
  Tail Core.Prim n a ->
  ExceptT (ReprimError n a) m (Term k X64.Prim n a -> Term k X64.Prim n a)
reprimTail an ns = \case
  Copy ac xs ->
    pure $
      Let an ns (Copy ac xs)

  Call ac n xs ->
    pure $
      Let an ns (Call ac n xs)

  Prim ap p0 xs -> do
    case reprimTrivial p0 of
      Just p ->
        pure $
          Let an ns (Prim ap p xs)
      Nothing ->
        reprimComplex an ns ap p0 xs

reprimTrivial :: Core.Prim -> Maybe X64.Prim
reprimTrivial = \case
    -- Trivial cases
    Core.Neg ->
      Just X64.Neg
    Core.Not ->
      Just X64.Not

    Core.Add ->
      Just X64.Add
    Core.Sub ->
      Just X64.Sub

    Core.And ->
      Just X64.And
    Core.Xor ->
      Just X64.Xor
    Core.Or ->
      Just X64.Or

    Core.Shl ->
      Just X64.Sal
    Core.Shr ->
      Just X64.Sar

    -- Complex cases, must be handled by reprimComplex.
    Core.Mul ->
      Nothing
    Core.Div ->
      Nothing
    Core.Mod ->
      Nothing

    Core.Eq ->
      Nothing
    Core.Ne ->
      Nothing
    Core.Lt ->
      Nothing
    Core.Le ->
      Nothing
    Core.Gt ->
      Nothing
    Core.Ge ->
      Nothing

reprimComplex ::
  FreshName n =>
  MonadFresh m =>
  a ->
  [n] ->
  a ->
  Core.Prim ->
  [Atom n a] ->
  ExceptT (ReprimError n a) m (Term k X64.Prim n a -> Term k X64.Prim n a)
reprimComplex an ns ap p xs =
  case (ns, p, xs) of

    -- Arithmetic --

    ([dst], Core.Mul, [x, y]) -> do
      ignore <- newFresh
      pure .
        Let an [dst, ignore] $
        Prim ap X64.Imul [x, y]

    ([dst], Core.Div, [x, y]) -> do
      ignore <- newFresh
      high_x <- newFresh
      pure $
        Let ap [high_x]
          (Prim ap X64.Cqto [x]) .
        Let an [dst, ignore]
          (Prim ap X64.Idiv [x, Variable ap high_x, y])

    ([dst], Core.Mod, [x, y]) -> do
      ignore <- newFresh
      high_x <- newFresh
      pure $
        Let ap [high_x]
          (Prim ap X64.Cqto [x]) .
        Let an [ignore, dst]
          (Prim ap X64.Idiv [x, Variable ap high_x, y])

    -- Relations --

    ([dst], prim, [x, y])
      | Just cc <- takeCC prim
      -> do
        flags <- freshen dst
        dst8 <- freshen dst
        pure $
          Let an [flags]
            (Prim ap X64.Cmp [x, y]) .
          Let an [dst8]
            (Prim ap (X64.Set cc) [Variable ap flags]) .
          Let an [dst]
            (Prim ap X64.Movzbq [Variable ap dst8])

    _ ->
      throwE $ ReprimInvalidPrim ns p xs

takeCC :: Core.Prim -> Maybe Cc
takeCC = \case
  Core.Eq ->
    Just E
  Core.Ne ->
    Just Ne
  Core.Lt ->
    Just L
  Core.Le ->
    Just Le
  Core.Gt ->
    Just G
  Core.Ge ->
    Just Ge
  _ ->
    Nothing
