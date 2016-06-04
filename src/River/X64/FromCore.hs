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
import           River.Core.Transform.Grail
import           River.Core.Transform.Jump
import           River.Core.Transform.Split
import           River.Fresh
import           River.X64.Color
import           River.X64.Name as X64
import qualified River.X64.Primitive as X64
import           River.X64.Syntax
import           River.X64.Transform.Recondition
import           River.X64.Transform.Reprim


data X64Error n a =
    ReprimError !(ReprimError n a)
  | GrailError !(GrailError n a)
  | SplitError !(SplitError n a)
  | RegisterAllocationError !(ColorError (RegisterError n) n)
  | CopyArityMismatch ![Operand64] !(Tail X64.Prim X64.Name a)
  | AssemblyInvalidPrim !X64.Prim ![Operand64] ![Operand64]
  | LabelCannotBeAtom !a !Label
  | CannotLetBindLabel !a !Label
  | MalformedIf !a !Cc !(Atom X64.Name a) !(Term Cc X64.Prim X64.Name a) !(Term Cc X64.Prim X64.Name a)
  | MalformedBinding !a !X64.Name !(Binding Cc X64.Prim X64.Name a)
  | CallNotSupportedYet !a !Label ![Atom X64.Name a]
  | CannotCallRegister !a !Register64 ![Atom X64.Name a]
    deriving (Eq, Ord, Show, Functor)

------------------------------------------------------------------------

assemblyOfProgram ::
  Ord n =>
  FreshName n =>
  (n -> Label) ->
  Program () Core.Prim n a ->
  Either (X64Error n a) [Instruction]
assemblyOfProgram mkLabel p0 = do
  let
    runFreshN p =
      runFreshFrom $ nextOfProgram p

  let
    p1 =
      reconditionProgram p0

  p2 <-
    first ReprimError . runFreshN p1 . runExceptT $
      jumpOfProgram =<< reprimProgram p1

  p3 <-
    first GrailError $
      grailOfProgram p2

  p4 <-
    first SplitError $
      splitOfProgram p3

  p5 <-
    first RegisterAllocationError $
      coloredOfProgram colorByRegister p4

  let
    p6 =
      coalesceProgram $ first (fromColored mkLabel) p5

  case p6 of
    Program _ tm ->
      assemblyOfTerm tm

fromColored :: (n -> Label) -> (n, Maybe Register64) -> X64.Name
fromColored f (n, mr) =
  case mr of
    Nothing ->
      Lb $ f n
    Just r ->
      Rg r

assemblyOfTerm :: Term Cc X64.Prim X64.Name a -> Either (X64Error n a) [Instruction]
assemblyOfTerm = \case
  Return _ (Copy _ [Variable _ (Rg RAX)]) ->
    pure [ Ret ]

  Return _ (Call _ (Lb n) _) ->
    pure [ Jmp n ]

  Return _ tl ->
    (++)
      <$> assemblyOfTail [Register64 RAX] tl
      <*> pure [ Ret ]

  If _ Nz (Variable _ (Rg i)) (Return _ (Call _ (Lb t) [])) e ->
    let
      preamble =
        [ Test (Register64 i) (Register64 i)
        , J Nz t ]
    in
      (preamble ++) <$> assemblyOfTerm e

  If a k i t e ->
    Left $ MalformedIf a k i t e

  Let a ns tl tm -> do
    ops <- operandsOfNames a ns
    (++)
      <$> assemblyOfTail ops tl
      <*> assemblyOfTerm tm

  LetRec _ bs tm ->
    (++)
      <$> assemblyOfTerm tm
      <*> assemblyOfBindings bs

assemblyOfBindings :: Bindings Cc X64.Prim X64.Name a -> Either (X64Error n a) [Instruction]
assemblyOfBindings = \case
  Bindings a nbs ->
    let
      go = \case
        (Lb n, b) ->
          fmap ([ Lbl n ] ++) (assemblyOfBinding b)

        (n, b) ->
          Left $ MalformedBinding a n b
    in
      concat <$> traverse go nbs

assemblyOfBinding :: Binding Cc X64.Prim X64.Name a -> Either (X64Error n a) [Instruction]
assemblyOfBinding = \case
  Lambda _ _ tm ->
    assemblyOfTerm tm

operandsOfNames :: a -> [X64.Name] -> Either (X64Error n a) [Operand64]
operandsOfNames a ns =
  let
    go = \case
      Lb l ->
        Left $ CannotLetBindLabel a l
      Rg r ->
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

    Call a (Lb n) xs ->
      -- TODO only tail calls are supports, which is handled in assemblyOfTerm
      Left $ CallNotSupportedYet a n xs

    Call a (Rg n) xs ->
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

      (X64.Movzbq, [x], [dst]) ->
        pure [ Movzbq x dst ]

      (X64.Cmp, [x, y], [Register64 RFLAGS]) ->
        pure [ Cmpq x y ]

      (X64.Set cc, [Register64 RFLAGS], [dst]) ->
        pure [ Set cc dst ]

      _ ->
        Left $ AssemblyInvalidPrim prim xs dsts


operandOfAtom :: Atom X64.Name a -> Either (X64Error n a) Operand64
operandOfAtom = \case
  Immediate _ x ->
    pure . Immediate64 $ fromInteger x
  Variable _ (Rg x) ->
    pure $ Register64 x
  Variable a (Lb x) ->
    Left $ LabelCannotBeAtom a x
