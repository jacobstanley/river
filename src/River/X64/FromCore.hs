{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
module River.X64.FromCore (
    assemblyOfProgram

  , X64Error(..)
  ) where

import           Control.Monad.Trans.Except (runExceptT)

import           Data.Bifunctor (first)

import           River.Core.Color
import           River.Core.Fresh
import qualified River.Core.Primitive as Core
import           River.Core.Syntax
import           River.Core.Transform.Coalesce
import           River.Core.Transform.Else
import           River.Core.Transform.Grail
import           River.Core.Transform.Split
import           River.Fresh
import           River.X64.Assimilate
import           River.X64.Color
import           River.X64.Name as X64
import qualified River.X64.Primitive as X64
import           River.X64.Syntax


data X64Error n a =
    AssimilateError !(AssimilateError n a)
  | GrailError !(GrailError n a)
  | SplitError !(SplitError n a)
  | RegisterAllocationError !(ColorError (RegisterError n) n)
  | CopyArityMismatch ![Operand64] !(Tail X64.Prim X64.Name a)
  | AssemblyInvalidPrim !X64.Prim ![Operand64] ![Operand64]
  | LabelCannotBeAtom !a !Label
  | CannotLetBindLabel !a !Label
  | MalformedIf !a !(Atom X64.Name a) !(Term X64.Prim X64.Name a) !(Term X64.Prim X64.Name a)
  | MalformedBinding !a !X64.Name !(Binding X64.Prim X64.Name a)
  | CallNotSupportedYet !a !Label ![Atom X64.Name a]
  | CannotCallRegister !a !Register64 ![Atom X64.Name a]
    deriving (Eq, Ord, Show, Functor)

------------------------------------------------------------------------

assemblyOfProgram ::
  Ord n =>
  FreshName n =>
  (n -> Label) ->
  Program Core.Prim n a ->
  Either (X64Error n a) [Instruction]
assemblyOfProgram mkLabel p0 = do
  let
    runFreshN p =
      runFreshFrom $ nextOfProgram p

  p1 <-
    first AssimilateError . runFreshN p0 . runExceptT $
      elseOfProgram =<< assimilateProgram p0

  p2 <-
    first GrailError $
      grailOfProgram p1

  p3 <-
    first SplitError $
      splitOfProgram p2

  p4 <-
    first RegisterAllocationError $
      coloredOfProgram colorByRegister p3

  let
    p5 =
      coalesceProgram $ first (fromColored mkLabel) p4

  case p5 of
    Program _ tm ->
      assemblyOfTerm tm

fromColored :: (n -> Label) -> (n, Maybe Register64) -> X64.Name
fromColored f (n, mr) =
  case mr of
    Nothing ->
      L $ f n
    Just r ->
      R r

assemblyOfTerm :: Term X64.Prim X64.Name a -> Either (X64Error n a) [Instruction]
assemblyOfTerm = \case
  Return _ (Copy _ [Variable _ (R RAX)]) ->
    pure [ Ret ]

  -- TODO Ensure calls are in Grail Normal Form (GNF) so we don't need to do
  -- TODO any extra moves at this time.
  Return _ (Call _ (L n) _) ->
    pure [ Jmp n ]

  Return _ tl ->
    (++)
      <$> assemblyOfTail [Register64 RAX] tl
      <*> pure [ Ret ]

  If _ (Variable _ (R i)) t (Return _ (Call _ (L e) [])) ->
    let
      preamble =
        [ Test (Register64 i) (Register64 i)
        , Jz e ]
    in
      (preamble ++) <$> assemblyOfTerm t

  If a i t e ->
    Left $ MalformedIf a i t e

  Let a ns tl tm -> do
    ops <- operandsOfNames a ns
    (++)
      <$> assemblyOfTail ops tl
      <*> assemblyOfTerm tm

  LetRec _ bs tm ->
    (++)
      <$> assemblyOfTerm tm
      <*> assemblyOfBindings bs

assemblyOfBindings :: Bindings X64.Prim X64.Name a -> Either (X64Error n a) [Instruction]
assemblyOfBindings = \case
  Bindings a nbs ->
    let
      go = \case
        (L n, b) ->
          fmap ([ Lbl n ] ++) (assemblyOfBinding b)

        (n, b) ->
          Left $ MalformedBinding a n b
    in
      concat <$> traverse go nbs

assemblyOfBinding :: Binding X64.Prim X64.Name a -> Either (X64Error n a) [Instruction]
assemblyOfBinding = \case
  Lambda _ _ tm ->
    assemblyOfTerm tm

operandsOfNames :: a -> [X64.Name] -> Either (X64Error n a) [Operand64]
operandsOfNames a ns =
  let
    go = \case
      L l ->
        Left $ CannotLetBindLabel a l
      R r ->
        Right $ Register64 r
  in
    traverse go ns

assemblyOfTail ::
  [Operand64] ->
  Tail X64.Prim X64.Name a ->
  Either (X64Error n a) [Instruction]
assemblyOfTail dsts tl =
  case tl of
    Copy _ xs0
      | length dsts == length xs0 -> do
        xs <- traverse operandOfAtom xs0
        pure $ zipWith Movq xs dsts
      | otherwise ->
        Left $ CopyArityMismatch dsts tl

    Call a (L n) xs ->
      -- TODO only tail calls are supports, which is handled in assemblyOfTerm
      Left $ CallNotSupportedYet a n xs

    Call a (R n) xs ->
      Left $ CannotCallRegister a n xs

    Prim _ prim xs0 -> do
      xs <- traverse operandOfAtom xs0
      assemblyOfPrim prim xs dsts

data Commutative =
    Commutative
  | NotCommutative
    deriving (Eq)

assemblyOfPrim ::
  X64.Prim ->
  [Operand64] ->
  [Operand64] ->
  Either (X64Error n a) [Instruction]
assemblyOfPrim prim xs dsts =
  let
    unary instr x dst =
      if dst == x then
        pure [ instr dst ]
      else
        pure [ Movq x dst
             , instr dst ]

    binary instr comm x y dst =
      if dst == y then
        pure [ instr x dst ]
      else if dst == x then
        case comm of
          Commutative ->
            pure [ instr y dst ]
          NotCommutative ->
            pure [ Movq x (Register64 R11)
                 , Movq y dst
                 , instr (Register64 R11) dst ]
      else
        pure [ Movq y dst
             , instr x dst ]
  in
    case (prim, xs, dsts) of
      (X64.Neg, [x], [dst]) ->
        unary Negq x dst

      (X64.Add, [x, y], [dst]) ->
        binary Addq Commutative x y dst

      (X64.Sub, [x, y], [dst]) ->
        binary Subq NotCommutative y x dst

      (X64.Imul, [Register64 RAX, y], [Register64 RAX, Register64 RDX]) ->
        pure [ Imulq y ]

      (X64.Idiv, [Register64 RAX, Register64 RDX, y], [Register64 RAX, Register64 RDX]) ->
        pure [ Idivq y ]

      (X64.Cqto, [Register64 RAX], [Register64 RDX]) ->
        pure [ Cqto ]

      _ ->
        Left $ AssemblyInvalidPrim prim xs dsts


operandOfAtom :: Atom X64.Name a -> Either (X64Error n a) Operand64
operandOfAtom = \case
  Immediate _ x ->
    pure . Immediate64 $ fromInteger x
  Variable _ (R x) ->
    pure $ Register64 x
  Variable a (L x) ->
    Left $ LabelCannotBeAtom a x
