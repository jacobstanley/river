{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
-- | Convert from core primitives to x86-64 primitives.
module River.X64.Assimilate (
    assimilateProgram
  , assimilateTerm

  , AssimilateError(..)
  ) where

import           Control.Monad.Trans.Except (ExceptT, throwE)

import           River.Bifunctor
import qualified River.Core.Primitive as Core
import           River.Core.Syntax
import           River.Fresh
import qualified River.X64.Primitive as X64


data AssimilateError n a =
    AssimilateInvalidPrim ![n] Core.Prim ![Atom n a]
    deriving (Eq, Ord, Show, Functor)

assimilateProgram ::
  FreshName n =>
  Program Core.Prim n a ->
  ExceptT (AssimilateError n a) Fresh (Program X64.Prim n a)
assimilateProgram = \case
  Program a tm ->
    Program a <$> assimilateTerm tm

assimilateTerm ::
  FreshName n =>
  Term Core.Prim n a ->
  ExceptT (AssimilateError n a) Fresh (Term X64.Prim n a)
assimilateTerm = \case
  Return ar (Call ac n xs) ->
    pure $
      Return ar (Call ac n xs)

  Return a tl -> do
    -- TODO should be based on arity of tail, not just [n]
    -- TODO need to do arity inference before this is possible.
    n <- newFresh
    let_tail <- assimilateTail a [n] tl
    pure . let_tail $
      Return a (Copy a [Variable a n])

  If a i t e -> do
    If a i
      <$> assimilateTerm t
      <*> assimilateTerm e

  Let a ns tl tm -> do
    let_tail <- assimilateTail a ns tl
    let_tail <$> assimilateTerm tm

  LetRec a bs tm ->
    LetRec a
      <$> assimilateBindings bs
      <*> assimilateTerm tm

assimilateBindings ::
  FreshName n =>
  Bindings Core.Prim n a ->
  ExceptT (AssimilateError n a) Fresh (Bindings X64.Prim n a)
assimilateBindings = \case
  Bindings a bs ->
    Bindings a <$> traverse (secondA assimilateBinding) bs

assimilateBinding ::
  FreshName n =>
  Binding Core.Prim n a ->
  ExceptT (AssimilateError n a) Fresh (Binding X64.Prim n a)
assimilateBinding = \case
  Lambda a ns tm ->
    Lambda a ns <$> assimilateTerm tm

assimilateTail ::
  FreshName n =>
  a ->
  [n] ->
  Tail Core.Prim n a ->
  ExceptT (AssimilateError n a) Fresh (Term X64.Prim n a -> Term X64.Prim n a)
assimilateTail an ns = \case
  Copy ac xs ->
    pure $
      Let an ns (Copy ac xs)

  Call ac n xs ->
    pure $
      Let an ns (Call ac n xs)

  Prim ap p0 xs -> do
    case assimilateTrivialPrim p0 of
      Just p ->
        pure $
          Let an ns (Prim ap p xs)
      Nothing ->
        assimilateComplexPrim an ns ap p0 xs

assimilateTrivialPrim :: Core.Prim -> Maybe X64.Prim
assimilateTrivialPrim = \case
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

    -- Complex cases, must be handled by assimilateComplexPrim.
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

assimilateComplexPrim ::
  FreshName n =>
  a ->
  [n] ->
  a ->
  Core.Prim ->
  [Atom n a] ->
  ExceptT
    (AssimilateError n a)
    Fresh
    (Term X64.Prim n a -> Term X64.Prim n a)
assimilateComplexPrim an ns ap p xs =
  case (ns, p, xs) of
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

    _ ->
      throwE $ AssimilateInvalidPrim ns p xs
