{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
--
-- | Split multi-variable copies in to single variable copies.
--
--   For example:
--
--     let x, y = a, b
--
--     ==>
--
--     let x = a
--     let y = b
--
module River.Core.Transform.Split (
    splitOfProgram
  , splitOfTerm

  , SplitError(..)
  ) where

import           River.Bifunctor
import           River.Core.Syntax


data SplitError n a =
    SplitCopyArityMismatch !a ![n] ![Atom n a]
    deriving (Eq, Ord, Show, Functor)

splitOfProgram :: Program p n a -> Either (SplitError n a) (Program p n a)
splitOfProgram = \case
  Program a tm ->
    Program a <$> splitOfTerm tm

splitOfTerm :: Term p n a -> Either (SplitError n a) (Term p n a)
splitOfTerm = \case
  Return a tl ->
    pure $ Return a tl

  If a i t e ->
    If a i
      <$> splitOfTerm t
      <*> splitOfTerm e

  Let at ns (Copy ac xs) tm ->
    if length ns /= length xs then
      Left $ SplitCopyArityMismatch at ns xs
    else do
      let
        lets_in =
          foldr (.) id $
          zipWith (\n x -> Let at [n] $ Copy ac [x]) ns xs
      lets_in
        <$> splitOfTerm tm

  Let a ns tl tm ->
    Let a ns tl
      <$> splitOfTerm tm

  LetRec a bs tm -> do
    LetRec a
      <$> splitOfBindings bs
      <*> splitOfTerm tm

splitOfBindings :: Bindings p n a -> Either (SplitError n a) (Bindings p n a)
splitOfBindings = \case
  Bindings a bs ->
    Bindings a <$>
      traverse (secondA splitOfBinding) bs

splitOfBinding :: Binding p n a -> Either (SplitError n a) (Binding p n a)
splitOfBinding = \case
  Lambda a ns tm ->
    Lambda a ns <$>
      splitOfTerm tm
