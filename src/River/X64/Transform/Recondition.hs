{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
--
-- | Convert from boolean conditionals to x86-64 condition codes.
--
module River.X64.Transform.Recondition (
    reconditionProgram
  , reconditionTerm
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map

import           River.Bifunctor
import           River.Core.Syntax
import           River.Fresh
import           River.X64.Primitive


data Cond n =
    Cond Cc !n
    deriving (Eq, Ord, Show)

reconditionProgram ::
  Ord n =>
  FreshName n =>
  MonadFresh m =>
  Program () Prim n a ->
  m (Program Cc Prim n a)
reconditionProgram = \case
  Program a tm ->
    Program a <$> reconditionTerm Map.empty tm

reconditionTerm ::
  Ord n =>
  FreshName n =>
  MonadFresh m =>
  Map n (Cond n) ->
  Term () Prim n a ->
  m (Term Cc Prim n a)
reconditionTerm env0 = \case
  Return a tl ->
    pure $
      Return a tl

  If a () (Variable ai ni) t0 e0 -> do
    t <- reconditionTerm env0 t0
    e <- reconditionTerm env0 e0

    case Map.lookup ni env0 of
      Nothing -> do
        nflags <- freshen ni
        pure $
          Let ai [nflags] (Prim ai Test [Variable ai ni, Variable ai ni]) $
          If a Nz (Variable ai nflags) t e

      Just (Cond cc nflags) ->
        pure $
          If a cc (Variable ai nflags) t e

  If _ () _ _ _ ->
    error "malformed if"

  Let a [n] p@(Prim _ (Set cc) [Variable _ flags]) tm ->
    let
      env =
        Map.insert n (Cond cc flags) env0
    in
      Let a [n] p
        <$> reconditionTerm env tm

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
        <$> reconditionTerm env tm

  Let a ns tl tm ->
    Let a ns tl
      <$> reconditionTerm env0 tm

  LetRec a bs tm ->
    LetRec a
      <$> reconditionBindings env0 bs
      <*> reconditionTerm env0 tm


reconditionBindings ::
  Ord n =>
  FreshName n =>
  MonadFresh m =>
  Map n (Cond n) ->
  Bindings () Prim n a ->
  m (Bindings Cc Prim n a)
reconditionBindings env0 = \case
  Bindings a bs ->
    Bindings a
      <$> traverse (secondA $ reconditionBinding env0) bs

reconditionBinding ::
  Ord n =>
  FreshName n =>
  MonadFresh m =>
  Map n (Cond n) ->
  Binding () Prim n a ->
  m (Binding Cc Prim n a)
reconditionBinding env0 = \case
  Lambda a ns tm ->
    Lambda a ns
      <$> reconditionTerm env0 tm
