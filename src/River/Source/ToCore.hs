{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module River.Source.ToCore (
    coreOfProgram
  , coreOfStatements
  , coreOfExpression
  , coreOfUnaryOp
  , coreOfBinaryOp
  ) where

import           Data.Text (Text)

import qualified River.Core.Syntax as Core
import           River.Fresh
import           River.Name
import           River.Source.Syntax


coreOfProgram :: Program a -> Core.Program (Maybe a) (Name Text)
coreOfProgram = \case
  Program a ss ->
    Core.Program (Just a) . runFresh $ coreOfStatements ss

coreOfStatements :: [Statement a] -> Fresh (Core.Term (Maybe a) (Name Text))
coreOfStatements = \case
  [] ->
    pure $
      Core.Return Nothing $
      Core.Copy Nothing []

  Declaration _ _ Nothing : ss ->
    coreOfStatements ss

  Declaration _ (Identifier n) (Just x) : ss -> do
    term_let <- coreOfExpression (Name n) x
    term_ss <- coreOfStatements ss

    pure $
      term_let
      term_ss

  Assignment _ (Identifier n) Nothing x : ss -> do
    term_let <- coreOfExpression (Name n) x
    term_ss <- coreOfStatements ss

    pure $
      term_let
      term_ss

  Assignment a i@(Identifier n) (Just op) x : ss -> do
    let
      bx =
        Binary a op (Variable a i) x

    term_let <- coreOfExpression (Name n) bx
    term_ss <- coreOfStatements ss

    pure $
      term_let
      term_ss

  Return a x : _ss -> do
    n <- newFresh
    term_let <- coreOfExpression n x

    pure $
      term_let .
      Core.Return (Just a) $
      Core.Copy (Just a) [Core.Variable (Just a) n]

coreOfExpression ::
  Name Text ->
  Expression a ->
  Fresh (Core.Term (Maybe a) (Name Text) -> Core.Term (Maybe a) (Name Text))
coreOfExpression n0 = \case
  Literal a x ->
    pure $
      Core.Let (Just a) [n0] $
      Core.Copy (Just a) [Core.Immediate (Just a) x]

  Variable a (Identifier n1) ->
    pure $
      Core.Let (Just a) [n0] $
      Core.Copy (Just a) [Core.Variable (Just a) (Name n1)]

  Unary a op x -> do
    n1 <- freshen n0
    term_let <- coreOfExpression n1 x

    let
      tail_op =
        Core.Unary
          (Just a)
          (coreOfUnaryOp op)
          (Core.Variable (Just a) n1)

    pure $
      term_let .
      Core.Let (Just a) [n0] tail_op

  Binary a op x y -> do
    n1 <- freshen n0
    n2 <- freshen n0
    term_letx <- coreOfExpression n1 x
    term_lety <- coreOfExpression n2 y

    let
      tail_op =
        Core.Binary
          (Just a)
          (coreOfBinaryOp op)
          (Core.Variable (Just a) n1)
          (Core.Variable (Just a) n2)

    pure $
      term_letx .
      term_lety .
      Core.Let (Just a) [n0] tail_op

coreOfUnaryOp :: UnaryOp -> Core.UnaryOp
coreOfUnaryOp = \case
  Neg ->
    Core.Neg

coreOfBinaryOp :: BinaryOp -> Core.BinaryOp
coreOfBinaryOp = \case
  Add ->
    Core.Add
  Sub ->
    Core.Sub
  Mul ->
    Core.Mul
  Div ->
    Core.Div
  Mod ->
    Core.Mod
