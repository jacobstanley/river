{-# LANGUAGE LambdaCase #-}
module River.Source.Elaborate (
    elaborateProgram
  ) where

import           Data.Maybe (maybeToList)

import           River.Source.Concrete.Annotation
import qualified River.Source.Concrete.Syntax as Concrete
import           River.Source.Syntax


elaborateProgram :: Concrete.Program a -> Program a
elaborateProgram = \case
  Concrete.Program a b ->
    Program a $ elaborateBlock b

elaborateBlock :: Concrete.Block a -> Block a
elaborateBlock = \case
  Concrete.Block a ss ->
    Block a $ elaborateStatements ss

elaborateStatements :: [Concrete.Statement a] -> [Statement a]
elaborateStatements = \case
  [] ->
    []

  Concrete.SSimple _ s : ss ->
    elaborateSimple s $ elaborateStatements ss

  Concrete.SControl _ c : ss ->
    elaborateControl c $ elaborateStatements ss

  Concrete.SBlock _ (Concrete.Block _ ss0) : ss ->
    elaborateStatements ss0 ++ elaborateStatements ss

elaborateControl :: Concrete.Control a -> [Statement a] -> [Statement a]
elaborateControl ctrl ss =
  case ctrl of
    Concrete.If a cond t me ->
      [ If a (elaborateExpression cond)
          (mkBlock a [t])
          (mkBlock a (maybeToList me)) ] ++ ss

    Concrete.While a cond s ->
      [ While a (elaborateExpression cond)
          (mkBlock a [s]) ] ++ ss

    Concrete.For a init0 cond step0 s ->
      let
        ss_init =
          maybe id elaborateSimple init0

        ss_step =
          maybe id elaborateSimple step0 []

        ss_body =
          elaborateStatements [s]
      in
        ss_init $
        [ While a (elaborateExpression cond)
            (Block a (ss_body ++ ss_step)) ] ++ ss

    Concrete.Return a x ->
      [ Return a (elaborateExpression x) ] ++ ss

mkBlock :: a -> [Concrete.Statement a] -> Block a
mkBlock a = \case
  [] ->
    Block a []
  s : ss ->
    Block (annotOfStatement s) $ elaborateStatements (s : ss)

elaborateSimple :: Concrete.Simple a -> [Statement a] -> [Statement a]
elaborateSimple simp ss =
  case simp of
    Concrete.Assign a lv asop x ->
      let
        ident =
          elaborateLValue lv

        var =
          Variable (annotOfLValue lv) ident
      in
        Assign a ident (elaborateAssignOp a var (elaborateExpression x) asop) : ss

    Concrete.Post a lv pop ->
      let
        ident =
          elaborateLValue lv

        var =
          Variable (annotOfLValue lv) ident
      in
        Assign a ident (elaboratePostOp a var pop) : ss

    Concrete.Declare a typ ident Nothing ->
      [ Declare a typ ident $
          Block a ss ]

    Concrete.Declare a typ ident (Just x) ->
      [ Declare a typ ident $
          Block a (Assign a ident (elaborateExpression x) : ss) ]

elaborateLValue :: Concrete.LValue a -> Identifier
elaborateLValue = \case
  Concrete.LIdentifier _ ident ->
    ident

elaborateExpression :: Concrete.Expression a -> Expression a
elaborateExpression = \case
  Concrete.Literal a lit ->
    Literal a lit
  Concrete.Variable a ident ->
    Variable a ident
  Concrete.Unary a op x ->
    Unary a op
      (elaborateExpression x)
  Concrete.Binary a op x y ->
    elaborateBinaryOp a
      (elaborateExpression x)
      (elaborateExpression y)
      op
  Concrete.Conditional a i t e ->
    Conditional a
      (elaborateExpression i)
      (elaborateExpression t)
      (elaborateExpression e)

elaborateBinaryOp :: a -> Expression a -> Expression a -> Concrete.BinaryOp -> Expression a
elaborateBinaryOp a x y = \case
  Concrete.Mul ->
    Binary a Mul x y
  Concrete.Div ->
    Binary a Div x y
  Concrete.Mod ->
    Binary a Mod x y
  Concrete.Add ->
    Binary a Add x y
  Concrete.Sub ->
    Binary a Sub x y
  Concrete.Shl ->
    Binary a Shl x y
  Concrete.Shr ->
    Binary a Shr x y
  Concrete.Lt ->
    Binary a Lt x y
  Concrete.Le ->
    Binary a Le x y
  Concrete.Gt ->
    Binary a Gt x y
  Concrete.Ge ->
    Binary a Ge x y
  Concrete.Eq ->
    Binary a Eq x y
  Concrete.NEq ->
    Binary a NEq x y
  Concrete.BAnd ->
    Binary a And x y
  Concrete.BXor ->
    Binary a Xor x y
  Concrete.BOr ->
    Binary a Or x y
  Concrete.LAnd ->
    Conditional a x y (Literal a $ LiteralBool False)
  Concrete.LOr ->
    Conditional a x (Literal a $ LiteralBool True) y

elaborateAssignOp :: a -> Expression a -> Expression a -> Concrete.AssignOp -> Expression a
elaborateAssignOp a var x = \case
  Concrete.AEq ->
    x
  Concrete.AAdd ->
    Binary a Add var x
  Concrete.ASub ->
    Binary a Sub var x
  Concrete.AMul ->
    Binary a Mul var x
  Concrete.ADiv ->
    Binary a Div var x
  Concrete.AMod ->
    Binary a Mod var x
  Concrete.AAnd ->
    Binary a And var x
  Concrete.AXor ->
    Binary a Xor var x
  Concrete.AOr ->
    Binary a Or var x
  Concrete.AShl ->
    Binary a Shl var x
  Concrete.AShr ->
    Binary a Shr var x

elaboratePostOp :: a -> Expression a -> Concrete.PostOp -> Expression a
elaboratePostOp a var = \case
  Concrete.Inc ->
    Binary a Add var (Literal a $ LiteralInt 1)
  Concrete.Dec ->
    Binary a Sub var (Literal a $ LiteralInt 1)
