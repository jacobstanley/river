{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
--
-- | Convert from boolean conditionals to x86-64 condition codes.
--
module River.X64.Transform.Recondition where

import           Data.Map (Map)
import qualified Data.Map as Map

import           River.Bifunctor
import           River.Core.Syntax
import           River.X64.Primitive


data Cond n =
    Cond Cc !n
    deriving (Eq, Ord, Show)

reconditionProgram :: Ord n => Program () Prim n a -> Program Cc Prim n a
reconditionProgram = \case
  Program a tm ->
    Program a $ reconditionTerm Map.empty tm

reconditionTerm :: Ord n => Map n (Cond n) -> Term () Prim n a -> Term Cc Prim n a
reconditionTerm env0 = \case
  Return a tl ->
    Return a tl

  If a () (Variable ai ni) t e ->
    case Map.lookup ni env0 of
      Nothing ->
        error "malformed if x2"
      Just (Cond cc nflags) ->
        If a cc (Variable ai nflags)
          (reconditionTerm env0 t)
          (reconditionTerm env0 e)

  If _ () _ _ _ ->
    error "malformed if"

  Let a [n] p@(Prim _ (Set cc) [Variable _ flags]) tm ->
    let
      env =
        Map.insert n (Cond cc flags) env0
    in
      Let a [n] p
        (reconditionTerm env tm)

  Let a [n] p@(Prim _ Movzbq [Variable _ x]) tm ->
    let
      env =
        case Map.lookup x env0 of
          Nothing ->
            env0
          Just cond ->
            Map.insert n cond env0
    in
      Let a [n] p
        (reconditionTerm env tm)

  Let a ns tl tm ->
    Let a ns tl
      (reconditionTerm env0 tm)

  LetRec a bs tm ->
    LetRec a
      (reconditionBindings env0 bs)
      (reconditionTerm env0 tm)


reconditionBindings :: Ord n => Map n (Cond n) -> Bindings () Prim n a -> Bindings Cc Prim n a
reconditionBindings env0 = \case
  Bindings a bs ->
    Bindings a $
      fmap (second $ reconditionBinding env0) bs

reconditionBinding :: Ord n => Map n (Cond n) -> Binding () Prim n a -> Binding Cc Prim n a
reconditionBinding env0 = \case
  Lambda a ns tm ->
    Lambda a ns
      (reconditionTerm env0 tm)
