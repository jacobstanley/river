{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
module River.Core.Scope (
    freeOfProgram
  , freeOfTerm
  , freeOfTail
  , freeOfAtom

  , Free(..)
  , annotFreeOfProgram
  , annotFreeOfTerm
  , annotFreeOfTail
  , annotFreeOfAtom
  ) where

import           Data.Set (Set)
import qualified Data.Set as Set

import           River.Core.Annotation
import           River.Core.Syntax

------------------------------------------------------------------------

freeOfProgram :: Ord n => Program p n a -> Set n
freeOfProgram = \case
  Program _ tm ->
    freeOfTerm tm

freeOfTerm :: Ord n => Term p n a -> Set n
freeOfTerm = \case
  Let _ ns tl tm ->
    Set.union
      (freeOfTail tl)
      (freeOfTerm tm `Set.difference` Set.fromList ns)
  Return _ tl ->
    freeOfTail tl

freeOfTail :: Ord n => Tail p n a -> Set n
freeOfTail = \case
  Copy _ xs ->
    Set.unions $ fmap freeOfAtom xs
  Prim _ _ xs ->
    Set.unions $ fmap freeOfAtom xs

freeOfAtom :: Atom n a -> Set n
freeOfAtom = \case
  Immediate _ _ ->
    Set.empty
  Variable _ n ->
    Set.singleton n

------------------------------------------------------------------------

data Free n a =
  Free {
      freeVars :: !(Set n)
    , freeTail :: !a
    } deriving (Eq, Ord, Show, Functor)

annotFreeOfProgram :: Ord n => Program p n a -> Program p n (Free n a)
annotFreeOfProgram = \case
  Program a tm0 ->
    let
      !tm =
        annotFreeOfTerm tm0

      !free =
        freeVars $ annotOfTerm tm
    in
      Program (Free free a) tm

annotFreeOfTerm :: Ord n => Term p n a -> Term p n (Free n a)
annotFreeOfTerm = \case
  Let a ns tl0 tm0 ->
    let
      !tl =
        annotFreeOfTail tl0

      !tm =
        annotFreeOfTerm tm0

      !free =
        Set.union
          (freeVars (annotOfTail tl))
          (freeVars (annotOfTerm tm) `Set.difference` Set.fromList ns)
    in
      Let (Free free a) ns tl tm

  Return a tl0 ->
    let
      !tl =
        annotFreeOfTail tl0

      !free =
        freeVars $ annotOfTail tl
    in
      Return (Free free a) tl

annotFreeOfTail :: Ord n => Tail p n a -> Tail p n (Free n a)
annotFreeOfTail = \case
  Copy a xs0 ->
    let
      !xs =
        fmap annotFreeOfAtom xs0

      !free =
        Set.unions $
        fmap (freeVars . annotOfAtom) xs
    in
      Copy (Free free a) xs

  Prim a p xs0 ->
    let
      !xs =
        fmap annotFreeOfAtom xs0

      !free =
        Set.unions $
        fmap (freeVars . annotOfAtom) xs
    in
      Prim (Free free a) p xs

annotFreeOfAtom :: Atom n a -> Atom n (Free n a)
annotFreeOfAtom = \case
  Immediate a i ->
    Immediate (Free Set.empty a) i
  Variable a n ->
    Variable (Free (Set.singleton n) a) n
