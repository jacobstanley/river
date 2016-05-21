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
import           River.Fresh
import           River.Name
import           River.X64.Assimilate
import           River.X64.Color
import qualified River.X64.Primitive as X64
import           River.X64.Syntax


data X64Error n a =
    AssimilateError !(AssimilateError n a)
  | RegisterAllocationError !(ColorError (RegisterError n) n)
  | CopyArityMismatch ![Operand64] !(Tail X64.Prim Register64 a)
  | AssemblyInvalidPrim !X64.Prim ![Operand64] ![Operand64]
    deriving (Eq, Ord, Show, Functor)

------------------------------------------------------------------------

assemblyOfProgram ::
  Ord n =>
  Program Core.Prim (Name n) a ->
  Either (X64Error (Name n) a) [Instruction]
assemblyOfProgram p0 = do
  let
    runFreshN =
      runFreshFrom $ nextOfProgram p0
  p1 <- first AssimilateError . runFreshN . runExceptT $ assimilateProgram p0
  p <- first RegisterAllocationError $ coloredOfProgram colorByRegister p1
  case p of
    Program _ tm ->
      fmap coalesce $
      assemblyOfTerm tm

assemblyOfTerm :: Term X64.Prim Register64 a -> Either (X64Error n a) [Instruction]
assemblyOfTerm = \case
  Let _ ns tl tm ->
    (++) <$> assemblyOfTail (fmap Register64 ns) tl <*> assemblyOfTerm tm
  Return _ tl ->
    (++) <$> assemblyOfTail [Register64 RAX] tl <*> pure [Ret]

assemblyOfTail ::
  [Operand64] ->
  Tail X64.Prim Register64 a ->
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


operandOfAtom :: Atom Register64 a -> Operand64
operandOfAtom = \case
  Immediate _ x ->
    Immediate64 $ fromInteger x
  Variable _ x ->
    Register64 x

------------------------------------------------------------------------

coalesce :: [Instruction] -> [Instruction]
coalesce =
  let
    go = \case
      [] ->
        []

      Movq x y : xs
        | x == y ->
          go xs

      Movq x0 y0 : Movq x1 y1 : xs
        | x0 == y1
        , y0 == x1 ->
          go $ Movq x0 y0 : xs

      x : xs ->
        x : go xs
  in
    go
