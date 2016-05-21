{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
module River.X64.FromCore (
    assemblyOfProgram

  , X64Error(..)
  ) where

import           Data.Bifunctor (first)

import           River.Core.Color
import           River.Core.Syntax
import           River.X64.Color
import           River.X64.Syntax


data X64Error n a =
    RegisterAllocationError !(ColorError (RegisterError n) n)
  | CopyArityMismatch ![Operand64] !(Tail Register64 a)
  | InvalidPrim !Prim ![Operand64] ![Operand64]
    deriving (Eq, Ord, Show, Functor)


assemblyOfProgram :: Ord n => Program n a -> Either (X64Error n a) [Instruction]
assemblyOfProgram p0 = do
  p <- first RegisterAllocationError $ coloredOfProgram colorByRegister p0
  case p of
    Program _ tm ->
      assemblyOfTerm tm

assemblyOfTerm :: Term Register64 a -> Either (X64Error n a) [Instruction]
assemblyOfTerm = \case
  Let _ ns tl tm ->
    (++) <$> assemblyOfTail (fmap Register64 ns) tl <*> assemblyOfTerm tm
  Return _ tl ->
    (++) <$> assemblyOfTail [Register64 RAX] tl <*> pure [Ret]

assemblyOfTail ::
  [Operand64] ->
  Tail Register64 a ->
  Either (X64Error n a) [Instruction]
assemblyOfTail dsts tl =
  case tl of
    Copy _ xs
      | length dsts == length xs ->
        pure $ zipWith Movq (fmap operandOfAtom xs) dsts
      | otherwise ->
        Left $ CopyArityMismatch dsts tl
    Prim _ prim xs ->
      assemblyOfPrim prim (fmap operandOfAtom xs) dsts

data Commutative =
    Commutative
  | NotCommutative
    deriving (Eq)

assemblyOfPrim ::
  Prim ->
  [Operand64] ->
  [Operand64] ->
  Either (X64Error n a) [Instruction]
assemblyOfPrim prim xs dsts =
  let
    binary instr comm x y dst =
      if dst == x then
        pure [ instr y x ]
      else if dst == y && comm == Commutative then
        pure [ instr x y ]
      else
        pure [ Movq y dst
             , instr x dst ]
  in
    case (prim, xs, dsts) of
      (Add, [x, y], [dst]) ->
        binary Addq Commutative x y dst
      (Sub, [x, y], [dst]) ->
        binary Subq NotCommutative x y dst
      (Mul, [x, y], [dst]) ->
        binary Mulq Commutative x y dst
      (DivMod, [Register64 RAX, y], [Register64 RAX, Register64 RDX]) ->
        pure [ Cqto
             , Idivq y ]
      _ ->
        Left $ InvalidPrim prim xs dsts


operandOfAtom :: Atom Register64 a -> Operand64
operandOfAtom = \case
  Immediate _ x ->
    Immediate64 $ fromInteger x
  Variable _ x ->
    Register64 x
