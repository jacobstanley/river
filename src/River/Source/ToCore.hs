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

import qualified River.Core.Primitive as Core
import qualified River.Core.Syntax as Core
import           River.Core.Transform.Rename
import           River.Fresh
import           River.Name
import           River.Source.Syntax


coreOfProgram :: Program a -> Core.Program Core.Prim (Name Text) (Maybe a)
coreOfProgram = \case
  Program a b ->
    runFresh $
      renameProgram =<< Core.Program (Just a) <$> coreOfBlock b

coreOfBlock :: Block a -> Fresh (Core.Term Core.Prim (Name Text) (Maybe a))
coreOfBlock = \case
  Block _ ss ->
    coreOfStatements ss

coreOfStatements :: [Statement a] -> Fresh (Core.Term Core.Prim (Name Text) (Maybe a))
coreOfStatements = \case
  [] ->
    pure $
      Core.Return Nothing $
      Core.Copy Nothing []

  Declare _ _ _ b : _ss ->
    coreOfBlock b

  Assign _ (Identifier n) x : ss -> do
    term_let <- coreOfExpression (Name n) x
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
  Fresh
    (Core.Term Core.Prim (Name Text) (Maybe a) -> Core.Term Core.Prim (Name Text) (Maybe a))
coreOfExpression dst = \case
  Literal a (LiteralInt x) ->
    pure $
      Core.Let (Just a) [dst] $
      Core.Copy (Just a) [Core.Immediate (Just a) x]

  Variable a (Identifier n1) ->
    pure $
      Core.Let (Just a) [dst] $
      Core.Copy (Just a) [Core.Variable (Just a) (Name n1)]

  Unary a op x -> do
    n1 <- freshen dst
    term_let <- coreOfExpression n1 x

    let
      tail_op =
        Core.Prim (Just a) (coreOfUnaryOp op)
          [ Core.Variable (Just a) n1 ]

    pure $
      term_let .
      Core.Let (Just a) [dst] tail_op

  Binary a op x y -> do
    n1 <- freshen dst
    n2 <- freshen dst
    term_letx <- coreOfExpression n1 x
    term_lety <- coreOfExpression n2 y

    let
      tail_op =
        Core.Prim (Just a) (coreOfBinaryOp op)
          [ Core.Variable (Just a) n1
          , Core.Variable (Just a) n2 ]

    pure $
      term_letx .
      term_lety .
      Core.Let (Just a) [dst] tail_op

coreOfUnaryOp :: UnaryOp -> Core.Prim
coreOfUnaryOp = \case
  Neg ->
    Core.Neg

coreOfBinaryOp :: BinaryOp -> Core.Prim
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
