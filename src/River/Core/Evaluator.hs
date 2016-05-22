{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
module River.Core.Evaluator (
    evaluateProgram
  , Value(..)
  , RuntimeError(..)
  ) where

import           Control.Spoon (spoon)

import           Data.Int (Int64)
import           Data.Map (Map)
import qualified Data.Map as Map

import           River.Core.Primitive
import           River.Core.Syntax

data RuntimeError n a =
    DivisionByZero !a ![n] !(Tail Prim n a)
  | LetArityMismatch !a ![n] ![Value]
  | UnboundVariable !a !n
  | ArithError !a !Prim ![Value]
  | DivideByZero !a !Prim ![Value]
  | InvalidPrimApp !a !Prim ![Value]
    deriving (Eq, Ord, Show, Functor)

data Value =
    VInt64 !Int64
    deriving (Eq, Ord, Show)

evaluateProgram :: Ord n => Program Prim n a -> Either (RuntimeError n a) [Value]
evaluateProgram = \case
  Program _ tm ->
    evaluateTerm Map.empty tm

evaluateTerm :: Ord n => Map n Value -> Term Prim n a -> Either (RuntimeError n a) [Value]
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

evaluateTail :: Ord n => Map n Value -> Tail Prim n a -> Either (RuntimeError n a) [Value]
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

    (Mul, [VInt64 x, VInt64 y]) ->
      pure [ VInt64 $ x * y ]

    (Div, [_, VInt64 0]) ->
      Left $ DivideByZero a p xs

    (Mod, [_, VInt64 0]) ->
      Left $ DivideByZero a p xs

    (Div, [VInt64 x, VInt64 y]) ->
      case spoon $ quot x y of
        Nothing ->
          -- TODO would be good to be more specific, maybe it's not so hard to
          -- TODO detect what kind of error we'll have
          Left $ ArithError a p xs
        Just d ->
          pure [ VInt64 d ]

    (Mod, [VInt64 x, VInt64 y]) ->
      case spoon $ rem x y of
        Nothing ->
          Left $ ArithError a p xs
        Just m ->
          pure [ VInt64 m ]

    _ ->
      Left $ InvalidPrimApp a p xs
