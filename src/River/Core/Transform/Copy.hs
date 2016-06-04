{-# LANGUAGE LambdaCase #-}
--
-- | Primitive argument safe copy propagation.
--
--   Copy propagation that doesn't touch the names of arguments to primitives.
--   This is useful because it can be applied after register allocation.
--
module River.Core.Transform.Copy (
    copyOfProgram
  , copyOfTerm
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import           River.Bifunctor
import           River.Core.Syntax
import           River.Map
import           River.Progress


copyOfProgram :: (Ord n, MonadProgress m) => Program k p n a -> m (Program k p n a)
copyOfProgram = \case
  Program a tm0 -> do
    Program a <$> copyOfTerm Map.empty tm0

copyOfTerm ::
  Ord n =>
  MonadProgress m =>
  Map n (Atom n a) ->
  Term k p n a ->
  m (Term k p n a)
copyOfTerm env0 = \case
  Return a tl ->
    pure $
      Return a tl

  If a k i t e ->
    If a k i
      <$> copyOfTerm env0 t
      <*> copyOfTerm env0 e

  Let a ns tl0 tm0 -> do
    tl <- copyOfTail env0 tl0

    let
      env =
        case tl of
          Copy _ xs ->
            Map.fromList (zip ns xs) `Map.union`
            removeStaleBindings ns env0
          Call _ _ _ ->
            removeStaleBindings ns env0
          Prim _ _ _ ->
            removeStaleBindings ns env0

    Let a ns tl <$> copyOfTerm env tm0

  LetRec a bs tm ->
    LetRec a
      <$> copyOfBindings env0 bs
      <*> copyOfTerm env0 tm

removeStaleBindings :: Ord n => [n] -> Map n (Atom n a) -> Map n (Atom n a)
removeStaleBindings ns env =
  let
    names =
      Set.fromList ns

    -- k is bound to one of the names which we are about to bind
    bound k =
      Set.member k names

    -- v references one of the names which we are about to bind
    references v =
      case v of
        Variable _ n ->
          Set.member n names
        Immediate _ _ ->
          False

    stale (k, v) =
      bound k || references v
  in
    Map.filterWithKey (curry $ not . stale) env

copyOfTail ::
  Ord n =>
  MonadProgress m =>
  Map n (Atom n a) ->
  Tail p n a ->
  m (Tail p n a)
copyOfTail env = \case
  Copy a xs ->
    Copy a <$> traverse (copyOfAtom env) xs
  Call a n xs ->
    Call a n <$> traverse (copyOfAtom env) xs
  Prim a p xs ->
    -- We can't change the names of arguments to primitives or this transform
    -- can't be used after register allocation.
    pure $
      Prim a p xs

copyOfAtom ::
  Ord n =>
  MonadProgress m =>
  Map n (Atom n a) ->
  Atom n a ->
  m (Atom n a)
copyOfAtom env = \case
  Immediate a i ->
    pure $ Immediate a i
  Variable an n ->
    case Map.lookup n env of
      Nothing ->
        pure $ Variable an n
      -- TODO I think propagating constants at this stage could be detrimental
      -- Just (Immediate _ _) ->
      --    pure $ Variable an n
      Just x ->
        progress x

copyOfBindings ::
  Ord n =>
  MonadProgress m =>
  Map n (Atom n a) ->
  Bindings k p n a ->
  m (Bindings k p n a)
copyOfBindings env0 = \case
  Bindings a bs ->
    let
      env =
        env0 `mapDifferenceList` fmap fst bs
    in
      Bindings a <$> traverse (secondA (copyOfBinding env)) bs

copyOfBinding ::
  Ord n =>
  MonadProgress m =>
  Map n (Atom n a) ->
  Binding k p n a ->
  m (Binding k p n a)
copyOfBinding env0 = \case
  Lambda a ns tm ->
    let
      env =
        env0 `mapDifferenceList` ns
    in
      Lambda a ns <$> copyOfTerm env tm
