{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
module River.X64.Assimilate (
    assimilateProgram
  , assimilateTerm

  , AssimilateError(..)
  ) where

import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except (ExceptT, throwE)

import qualified River.Core.Primitive as Core
import           River.Core.Syntax
import           River.Fresh
import           River.Name
import qualified River.X64.Primitive as X64


data AssimilateError n a =
    AssimilateInvalidPrim ![n] Core.Prim ![Atom n a]
    deriving (Eq, Ord, Show, Functor)

assimilateProgram ::
  Program Core.Prim (Name n) a ->
  ExceptT (AssimilateError (Name n) a) Fresh (Program X64.Prim (Name n) a)
assimilateProgram = \case
  Program a tm ->
    Program a <$> assimilateTerm tm

assimilateTerm ::
  Term Core.Prim (Name n) a ->
  ExceptT (AssimilateError (Name n) a) Fresh (Term X64.Prim (Name n) a)
assimilateTerm = \case
  Let a ns tl tm -> do
    let_in <- assimilateTail a ns tl
    let_in <$> assimilateTerm tm

  Return a tl -> do
    -- TODO should be based on arity of tail, not just [n]
    n <- lift newFresh
    let_in <- assimilateTail a [n] tl
    pure . let_in $
      Return a (Copy a [Variable a n])

assimilateTail ::
  a ->
  [Name n] ->
  Tail Core.Prim (Name n) a ->
  ExceptT
    (AssimilateError (Name n) a)
    Fresh
    (Term X64.Prim (Name n) a -> Term X64.Prim (Name n) a)
assimilateTail an ns = \case
  Copy ac xs ->
    pure $
      Let an ns (Copy ac xs)

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
    Core.Add ->
      Just X64.Add
    Core.Sub ->
      Just X64.Sub

    -- Complex cases, must be handled by assimilateComplexPrim.
    Core.Mul ->
      Nothing
    Core.Div ->
      Nothing
    Core.Mod ->
      Nothing

assimilateComplexPrim ::
  a ->
  [Name n] ->
  a ->
  Core.Prim ->
  [Atom (Name n) a] ->
  ExceptT
    (AssimilateError (Name n) a)
    Fresh
    (Term X64.Prim (Name n) a -> Term X64.Prim (Name n) a)
assimilateComplexPrim an ns ap p xs =
  case (ns, p, xs) of
    ([dst], Core.Mul, [x, y]) -> do
      ignore <- lift newFresh
      pure .
        Let an [dst, ignore] $
        Prim ap X64.Imul [x, y]

    ([dst], Core.Div, [x, y]) -> do
      ignore <- lift newFresh
      high_x <- lift newFresh
      pure $
        Let ap [high_x]
          (Prim ap X64.Cqto [x]) .
        Let an [dst, ignore]
          (Prim ap X64.Idiv [x, Variable ap high_x, y])

    ([dst], Core.Mod, [x, y]) -> do
      ignore <- lift newFresh
      high_x <- lift newFresh
      pure $
        Let ap [high_x]
          (Prim ap X64.Cqto [x]) .
        Let an [ignore, dst]
          (Prim ap X64.Idiv [x, Variable ap high_x, y])

    _ ->
      throwE $ AssimilateInvalidPrim ns p xs
