{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module River.Source.ToCore (
    coreOfProgram
  , coreOfStatements
  , coreOfExpression
  , coreOfUnaryOp
  , coreOfBinaryOp
  ) where

import qualified Data.Set as Set
import           Data.Text (Text)

import qualified River.Core.Primitive as Core
import           River.Core.Scope
import qualified River.Core.Syntax as Core
import           River.Core.Transform.Rename
import           River.Fresh
import           River.Name
import           River.Source.Syntax


coreOfProgram :: Program a -> Core.Program Core.Prim (Name Text) (Maybe a)
coreOfProgram = \case
  Program a b ->
    runFresh $
      let
        finish =
          Core.Copy Nothing []
      in
        renameProgram =<< Core.Program (Just a) <$> coreOfBlock finish b

coreOfBlock ::
  Core.Tail Core.Prim (Name Text) (Maybe a) ->
  Block a ->
  Fresh (Core.Term Core.Prim (Name Text) (Maybe a))
coreOfBlock finish = \case
  Block _ ss ->
    coreOfStatements finish ss

coreOfStatements ::
  Core.Tail Core.Prim (Name Text) (Maybe a) ->
  [Statement a] ->
  Fresh (Core.Term Core.Prim (Name Text) (Maybe a))
coreOfStatements finish = \case
  [] ->
    pure $
      Core.Return Nothing finish

  Declare _ _ _ b : _ss ->
    coreOfBlock finish b

  Assign _ (Identifier n) x : ss -> do
    term_let <- coreOfExpression (Name n) x
    term_ss <- coreOfStatements finish ss

    pure $
      term_let
      term_ss

  If a i t e : ss -> do
    rest <- newFresh
    term_ss <- coreOfStatements finish ss

    let
      free =
        Set.toList $ freeOfTerm term_ss

      bindings =
        Core.Bindings Nothing
        [(rest, Core.Lambda Nothing free term_ss)]

      call =
        Core.Call Nothing rest $
        fmap (Core.Variable Nothing) free

    n <- newFresh
    term_let_if <- coreOfExpression n i
    term_then <- coreOfBlock call t
    term_else <- coreOfBlock call e

    pure $
      Core.LetRec Nothing bindings $
      term_let_if $
      Core.If (Just a) (Core.Variable (Just a) n)
        term_then
        term_else

  While a x b : ss -> do
    rest <- newFresh
    term_rest <- coreOfStatements finish ss

    let
      free_rest =
        Set.toList $ freeOfTerm term_rest

      term_call_rest =
        Core.Return Nothing $
        Core.Call Nothing rest $
        fmap (Core.Variable Nothing) free_rest

    n <- newFresh
    term_let_if <- coreOfExpression n x

    -- TODO this could be optimised, we're stupidly generating the core twice
    free_body <- Set.toList . freeOfTerm <$> coreOfBlock (Core.Copy Nothing []) b
    while <- newFresh

    let
      call_while =
        Core.Call (Just a) while $
        fmap (Core.Variable Nothing) free_body

    term_body <- coreOfBlock call_while b

    let
      term_while =
        term_let_if $
        Core.If (Just a) (Core.Variable (Just a) n)
          term_body
          term_call_rest

      free_while =
        Set.toList $ freeOfTerm term_while

      bindings =
        Core.Bindings Nothing
        [ (while, Core.Lambda Nothing free_while term_while)
        , (rest, Core.Lambda Nothing free_rest term_rest)
        ]

    pure $
      Core.LetRec Nothing bindings $
      Core.Return Nothing call_while

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
    (Core.Term Core.Prim (Name Text) (Maybe a) ->
     Core.Term Core.Prim (Name Text) (Maybe a))
coreOfExpression dst = \case
  Literal a (LiteralInt x) ->
    pure $
      Core.Let (Just a) [dst] $
      Core.Copy (Just a) [Core.Immediate (Just a) x]

  Literal a (LiteralBool b) ->
    let
      x =
        if b then 1 else 0
    in
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
  LNot ->
    Core.Not
  BNot ->
    Core.Not

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

  Eq ->
    Core.Eq
  Ne ->
    Core.Ne
  Lt ->
    Core.Lt
  Le ->
    Core.Le
  Gt ->
    Core.Gt
  Ge ->
    Core.Ge

  And ->
    Core.And
  Xor ->
    Core.Xor
  Or ->
    Core.Or

  Shl ->
    Core.Shl
  Shr ->
    Core.Shr
