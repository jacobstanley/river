{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
--
-- | Copy all arguments to a function in to variables which are named the same
--   as the function's parameters. This is known as Grail Normal Form [1] and
--   is the ANF equivalent of phi-elimination on SSA.
--
--   1. Grail: a functional form for imperative mobile code.
--      Lennart Beringer, Kenneth MacKenzie and Ian Stark
--      http://homepages.inf.ed.ac.uk/stark/graffi.pdf
--
module River.Core.Transform.Grail (
    grailOfProgram
  , grailOfTerm

  , GrailError(..)
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map

import           River.Bifunctor
import           River.Core.Syntax


data GrailError n a =
    GrailFunctionNotFound !a !n ![Atom n a]
  | GrailCallArityMismatch !a !n ![Atom n a] ![n]
    deriving (Eq, Ord, Show, Functor)

grailOfProgram :: Ord n => Program p n a -> Either (GrailError n a) (Program p n a)
grailOfProgram = \case
  Program a tm ->
    Program a <$> grailOfTerm Map.empty tm

grailOfTerm :: Ord n => Map n [n] -> Term p n a -> Either (GrailError n a) (Term p n a)
grailOfTerm env0 = \case
  Return a tl ->
    grailOfTail env0 tl $ pure . Return a

  If a i t e ->
    If a i
      <$> grailOfTerm env0 t
      <*> grailOfTerm env0 e

  Let a ns tl0 tm ->
    grailOfTail env0 tl0 $ \tl ->
      Let a ns tl <$>
        grailOfTerm env0 tm

  LetRec a bs0 tm -> do
    (env, bs) <- grailOfBindings env0 bs0
    LetRec a bs <$>
      grailOfTerm env tm

grailOfBindings ::
  Ord n =>
  Map n [n] ->
  Bindings p n a ->
  Either (GrailError n a) (Map n [n], Bindings p n a)
grailOfBindings env0 = \case
  Bindings a bs ->
    let
      env1 =
        Map.fromList $ fmap (second paramsOfBinding) bs

      env =
        env1 `Map.union` env0
    in
      (env,) . Bindings a <$>
        traverse (secondA (grailOfBinding env)) bs

paramsOfBinding :: Binding p n a -> [n]
paramsOfBinding = \case
  Lambda _ ns _ ->
    ns

grailOfBinding :: Ord n => Map n [n] -> Binding p n a -> Either (GrailError n a) (Binding p n a)
grailOfBinding env = \case
  Lambda a ns tm ->
    Lambda a ns <$> grailOfTerm env tm

grailOfTail ::
  Ord n =>
  Map n [n] ->
  Tail p n a ->
  (Tail p n a -> Either (GrailError n a) (Term p n a)) ->
  Either (GrailError n a) (Term p n a)
grailOfTail env tl mkTerm =
  case tl of
    Copy a xs ->
      mkTerm $ Copy a xs

    Call a n xs ->
      case Map.lookup n env of
        Nothing ->
          Left $ GrailFunctionNotFound a n xs
        Just ns ->
          if length ns /= length xs then
            Left $ GrailCallArityMismatch a n xs ns
          else
            Let a ns (Copy a xs) <$>
            mkTerm (Call a n $ fmap (Variable a) ns)

    Prim a n xs ->
      mkTerm $ Prim a n xs
