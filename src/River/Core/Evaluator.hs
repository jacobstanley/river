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

import           River.Bifunctor
import           River.Core.Primitive
import           River.Core.Syntax

data RuntimeError n a =
    DivisionByZero !a ![n] !(Tail Prim n a)
  | LetArityMismatch !a ![n] ![Value n a]
  | LambdaArityMismatch !a ![n] ![Value n a]
  | UnboundVariable !a !n
  | UnboundFunction !a !n
  | ArithError !a !Prim ![Value n a]
  | DivideByZero !a !Prim ![Value n a]
  | InvalidPrimApp !a !Prim ![Value n a]
  | CannotIfNonBool !a !(Value n a)
  | CannotCallNonLambda !a !n !(Value n a) ![Value n a]
    deriving (Eq, Ord, Show, Functor)

data Value n a =
    VInt64 !Int64
  | VLambda ![n] !(Term () Prim n a)
    deriving (Eq, Ord, Show, Functor)

evaluateProgram :: Ord n => Program () Prim n a -> Either (RuntimeError n a) [Value n a]
evaluateProgram = \case
  Program _ tm ->
    evaluateTerm Map.empty tm

evaluateTerm :: Ord n => Map n (Value n a) -> Term () Prim n a -> Either (RuntimeError n a) [Value n a]
evaluateTerm env0 = \case
  Return _ tl ->
    evaluateTail env0 tl

  If a () i0 t e -> do
    i <- evaluateAtom env0 i0
    case i of
      VInt64 0 ->
        evaluateTerm env0 e
      VInt64 _ ->
        evaluateTerm env0 t
      _ ->
        Left $ CannotIfNonBool a i

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

  LetRec _ (Bindings _ bs) tm ->
    let
      env1 =
        Map.fromList $
        fmap (second evaluateBinding) bs

      env =
        env1 `Map.union`
        env0
    in
      evaluateTerm env tm

evaluateBinding :: Binding () Prim n a -> Value n a
evaluateBinding = \case
  Lambda _ ns tm ->
    VLambda ns tm

evaluateTail :: Ord n => Map n (Value n a) -> Tail Prim n a -> Either (RuntimeError n a) [Value n a]
evaluateTail env = \case
  Copy _ xs ->
    traverse (evaluateAtom env) xs

  Call a n xs -> do
    vs <- traverse (evaluateAtom env) xs
    evaluateCall env a n vs

  Prim a p xs ->
    evaluatePrim a p =<< traverse (evaluateAtom env) xs

evaluateCall ::
  Ord n =>
  Map n (Value n a) ->
  a ->
  n ->
  [Value n a] ->
  Either (RuntimeError n a) [Value n a]
evaluateCall env0 a n vs =
  case Map.lookup n env0 of
    Nothing ->
      Left $ UnboundFunction a n
    Just (VLambda ns tm) ->
      -- TODO this is the same as a Let above, maybe extract?
      if length ns /= length vs then
        Left $ LambdaArityMismatch a ns vs
      else
        let
          env =
            Map.fromList (zip ns vs) `Map.union` env0
        in
          evaluateTerm env tm
    Just v ->
      Left $ CannotCallNonLambda a n v vs

evaluateAtom :: Ord n => Map n (Value n a) -> Atom n a -> Either (RuntimeError n a) (Value n a)
evaluateAtom env = \case
  Immediate _ x ->
    pure . VInt64 $ fromInteger x

  Variable a n ->
    case Map.lookup n env of
      Nothing ->
        Left $ UnboundVariable a n
      Just v ->
        pure v

evaluatePrim :: a -> Prim -> [Value n a] -> Either (RuntimeError n a) [Value n a]
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
