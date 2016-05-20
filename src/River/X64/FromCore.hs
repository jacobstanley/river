{-# LANGUAGE LambdaCase #-}
module River.X64.FromCore (
    assemblyOfProgram
  ) where

import           Data.Bifunctor (first)

import           River.Core.Color
import           River.Core.Syntax
import           River.X64.Color
import           River.X64.Syntax


data X64Error n a =
    RegisterAllocationError !(ColorError (RegisterError n) n)
  | InvalidTerm !(Term Register64 a)
  | InvalidTail1 !(Tail Register64 a)
    deriving (Eq, Ord, Show)


assemblyOfProgram :: Ord n => Program n a -> Either (X64Error n a) [Instruction]
assemblyOfProgram p0 = do
  p <- first RegisterAllocationError $ coloredOfProgram colorByRegister p0
  case p of
    Program _ tm ->
      assemblyOfTerm tm

assemblyOfTerm :: Term Register64 a -> Either (X64Error n a) [Instruction]
assemblyOfTerm = \case
  Let _ [n] tl tm ->
    (++) <$> assemblyOfTerm1 (Register64 n) tl <*> assemblyOfTerm tm
  Return _ tl ->
    (++) <$> assemblyOfTerm1 (Register64 RAX) tl <*> pure [Ret]
  tm ->
    Left $ InvalidTerm tm

assemblyOfTerm1 :: Operand64 -> Tail Register64 a -> Either (X64Error n a) [Instruction]
assemblyOfTerm1 n = \case
  Copy _ [x] ->
    pure [ Movq (operandOfAtom x) n ]
  Binary _ Add x y ->
    let
      xo =
        operandOfAtom x
      yo =
        operandOfAtom y
    in
      if xo == n then
        pure [ Addq (operandOfAtom y) (operandOfAtom x) ]
      else if yo == n then
        pure [ Addq (operandOfAtom x) (operandOfAtom y) ]
      else
        pure [ Movq (operandOfAtom y) n
             , Addq (operandOfAtom x) n ]
  tl ->
    Left $ InvalidTail1 tl

operandOfAtom :: Atom Register64 a -> Operand64
operandOfAtom = \case
  Immediate _ x ->
    Immediate64 $ fromInteger x
  Variable _ x ->
    Register64 x
