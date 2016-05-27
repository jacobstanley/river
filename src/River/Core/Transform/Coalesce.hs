{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module River.Core.Transform.Coalesce (
    coalesceProgram
  , coalesceTerm
  ) where

import           Data.Functor (($>))
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import           River.Bifunctor
import           River.Core.Annotation
import           River.Core.Scope
import           River.Core.Syntax


coalesceProgram :: Ord n => Program p n a -> Program p n a
coalesceProgram = \case
  Program a tm ->
    Program a .
    fmap freeTail .
    coalesceTerm Map.empty $
    annotFreeOfTerm tm

coalesceTerm ::
  Ord n =>
  Map n (Atom n (Free n a)) ->
  Term p n (Free n a) ->
  Term p n (Free n a)
coalesceTerm env0 tm0 =
  let
    nextEnv ns =
      let
        names =
          Set.fromList ns

        -- k is bound to one of the names which we are binding
        bound k =
          Set.member k names

        -- v references one of the names which we are binding
        references v =
          case v of
            Variable _ n ->
              Set.member n names
            Immediate _ _ ->
              False

        fresh k v =
          not (bound k || references v)
      in
        Map.filterWithKey fresh env0
  in
    case tm0 of
      Let a ns (Copy ac xs) tm ->
        let
          -- %rax = %rax
          sameRegister (n, x) =
            case x of
              Variable _ xv ->
                n == xv
              _ ->
                False

          -- %rax = %rcx
          -- %rax = %rcx
          alreadyContains (n, x) =
            case Map.lookup n env0 of
              Just nx ->
                fmap ($> ()) x ==
                fmap ($> ()) nx
              Nothing ->
                False

          -- %rax = %rcx
          -- %rcx = %rax
          pointsBack (n, x) =
            case x of
              Variable _ m ->
                case Map.lookup m env0 of
                  Just (Variable _ mv) ->
                    n == mv
                  _ ->
                    False
              Immediate _ _ ->
                False

          usedRegisters =
            freeVars $ annotOfTerm tm

          -- %rax = %rcx
          -- ** rax not used again
          deadRegister (n, _) =
            not $ Set.member n usedRegisters

          same nx = or
            [ sameRegister nx
            , alreadyContains nx
            , pointsBack nx
            , deadRegister nx
            ]

          env =
            Map.fromList nxs `Map.union`
            nextEnv ns

          nxs =
            zip ns xs
        in
          if all same nxs then
            coalesceTerm env0 tm
          else
            Let a ns (Copy ac xs) $
            coalesceTerm env tm

      Let a ns tl tm ->
        Let a ns tl $
        coalesceTerm (nextEnv ns) tm

      Return a tl ->
        Return a tl

      If a i t e ->
        If a i
          (coalesceTerm env0 t)
          (coalesceTerm env0 e)

      LetRec a bs tm ->
        LetRec a
          (coalesceBindings env0 bs)
          (coalesceTerm env0 tm)

coalesceBindings ::
  Ord n =>
  Map n (Atom n (Free n a)) ->
  Bindings p n (Free n a) ->
  Bindings p n (Free n a)
coalesceBindings env = \case
  Bindings a bs ->
    Bindings a $ fmap (second (coalesceBinding env)) bs

coalesceBinding ::
  Ord n =>
  Map n (Atom n (Free n a)) ->
  Binding p n (Free n a) ->
  Binding p n (Free n a)
coalesceBinding env = \case
  -- TODO dead parameter removal, probably a separate pass
  Lambda a ns tm ->
    Lambda a ns (coalesceTerm env tm)
