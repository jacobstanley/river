{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
module River.Core.Evaluator (
    evaluateProgram
  , RuntimeError(..)
  ) where

import           Data.Bits (shiftR)
import           Data.Int (Int64)
import           Data.Map (Map)
import qualified Data.Map as Map

import           River.Core.Syntax

data RuntimeError n a =
    DivisionByZero !a ![n] !(Tail n a)
  | LetArityMismatch !a ![n] ![Value]
  | UnboundVariable !a !n
  | DivideByZero !a !Prim ![Value]
  | InvalidPrimApp !a !Prim ![Value]
    deriving (Eq, Ord, Show, Functor)

data Value =
    VInt64 !Int64
    deriving (Eq, Ord, Show)

evaluateProgram :: Ord n => Program n a -> Either (RuntimeError n a) [Value]
evaluateProgram = \case
  Program _ tm ->
    evaluateTerm Map.empty tm

evaluateTerm :: Ord n => Map n Value -> Term n a -> Either (RuntimeError n a) [Value]
evaluateTerm env0 = \case
  Let a ns tl tm -> do
    vs <- evaluateTail env0 tl
    if length ns /= length vs then
      Left $ LetArityMismatch a ns vs
    else
      let
        env =
          Map.fromList (zip ns vs) `Map.union` env0
      in
        evaluateTerm env tm

  Return _ tl ->
    evaluateTail env0 tl

evaluateTail :: Ord n => Map n Value -> Tail n a -> Either (RuntimeError n a) [Value]
evaluateTail env = \case
  Copy _ xs ->
    traverse (evaluateAtom env) xs
  Prim a p xs ->
    evaluatePrim a p =<< traverse (evaluateAtom env) xs

evaluateAtom :: Ord n => Map n Value -> Atom n a -> Either (RuntimeError n a) Value
evaluateAtom env = \case
  Immediate _ x ->
    pure . VInt64 $ fromInteger x
  Variable a n ->
    case Map.lookup n env of
      Nothing ->
        Left $ UnboundVariable a n
      Just v ->
        pure v

evaluatePrim :: a -> Prim -> [Value] -> Either (RuntimeError n a) [Value]
evaluatePrim a p xs =
  case (p, xs) of
    (Neg, [VInt64 x]) ->
      pure [ VInt64 $ -x ]

    (Add, [VInt64 x, VInt64 y]) ->
      pure [ VInt64 $ x + y ]

    (Sub, [VInt64 x, VInt64 y]) ->
      pure [ VInt64 $ x - y ]

    (Mul, [VInt64 x0, VInt64 y0]) ->
      let
        x = fromIntegral x0
        y = fromIntegral y0
        r = x * y
      in
        pure [ VInt64 . fromInteger $ r
             , VInt64 . fromInteger $ r `shiftR` 64 ]

    (DivMod, [_, VInt64 0]) ->
      Left $ DivideByZero a p xs

    (DivMod, [VInt64 x, VInt64 y]) ->
      let
        (d, m) = divMod x y
      in
        pure [ VInt64 d
             , VInt64 m ]

    _ ->
      Left $ InvalidPrimApp a p xs
