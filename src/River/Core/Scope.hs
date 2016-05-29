{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
module River.Core.Scope (
    freeOfProgram
  , freeOfTerm
  , freeOfTail
  , freeOfAtom
  , freeOfBindings
  , freeOfBinding
  , boundOfBindings

  , Free(..)
  , annotFreeOfProgram
  , annotFreeOfTerm
  , annotFreeOfTail
  , annotFreeOfAtom
  ) where

import           Data.Bifunctor (second)
import           Data.Set (Set)
import qualified Data.Set as Set

import           River.Core.Annotation
import           River.Core.Syntax

------------------------------------------------------------------------

freeOfProgram :: Ord n => Program k p n a -> Set n
freeOfProgram = \case
  Program _ tm ->
    freeOfTerm tm

freeOfTerm :: Ord n => Term k p n a -> Set n
freeOfTerm = \case
  Return _ tl ->
    freeOfTail tl

  If _ _ i t e ->
    freeOfAtom i `Set.union`
    freeOfTerm t `Set.union`
    freeOfTerm e

  Let _ ns tl tm ->
    Set.union
      (freeOfTail tl)
      (freeOfTerm tm `Set.difference` Set.fromList ns)

  LetRec _ bs tm ->
    Set.difference
      (freeOfBindings bs `Set.union` freeOfTerm tm)
      (boundOfBindings bs)

boundOfBindings :: Ord n => Bindings k p n a -> Set n
boundOfBindings = \case
  Bindings _ bs ->
    Set.fromList $ fmap fst bs

freeOfBindings :: Ord n => Bindings k p n a -> Set n
freeOfBindings = \case
  Bindings _ bs ->
    Set.unions $ fmap (freeOfBinding . snd) bs

freeOfBinding :: Ord n => Binding k p n a -> Set n
freeOfBinding = \case
  Lambda _ ns tm ->
    freeOfTerm tm `Set.difference` Set.fromList ns

freeOfTail :: Ord n => Tail p n a -> Set n
freeOfTail = \case
  Copy _ xs ->
    Set.unions $ fmap freeOfAtom xs
  Call _ _ xs ->
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

annotFreeOfProgram :: Ord n => Program k p n a -> Program k p n (Free n a)
annotFreeOfProgram = \case
  Program a tm0 ->
    let
      tm =
        annotFreeOfTerm tm0

      free =
        freeVars $ annotOfTerm tm
    in
      Program (Free free a) tm

annotFreeOfTerm :: Ord n => Term k p n a -> Term k p n (Free n a)
annotFreeOfTerm = \case
  Return a tl0 ->
    let
      tl =
        annotFreeOfTail tl0

      free =
        freeVars $ annotOfTail tl
    in
      Return (Free free a) tl

  If a k i0 t0 e0 ->
    let
      i =
        annotFreeOfAtom i0

      t =
        annotFreeOfTerm t0

      e =
        annotFreeOfTerm e0

      free =
        freeVars (annotOfAtom i) `Set.union`
        freeVars (annotOfTerm t) `Set.union`
        freeVars (annotOfTerm e)
    in
      If (Free free a) k i t e

  Let a ns tl0 tm0 ->
    let
      tl =
        annotFreeOfTail tl0

      tm =
        annotFreeOfTerm tm0

      free =
        Set.union
          (freeVars (annotOfTail tl))
          (freeVars (annotOfTerm tm) `Set.difference` Set.fromList ns)
    in
      Let (Free free a) ns tl tm

  LetRec a bs0 tm0 ->
    let
      bs =
        annotFreeOfBindings bs0

      tm =
        annotFreeOfTerm tm0

      free =
        Set.difference
          (freeVars (annotOfBindings bs) `Set.union` freeVars (annotOfTerm tm))
          (boundOfBindings bs)
    in
      LetRec (Free free a) bs tm

annotFreeOfBindings :: Ord n => Bindings k p n a -> Bindings k p n (Free n a)
annotFreeOfBindings = \case
  Bindings a bs0 ->
    let
      bs =
        fmap (second annotFreeOfBinding) bs0

      free =
        Set.unions $ fmap (freeVars . annotOfBinding . snd) bs
    in
      Bindings (Free free a) bs

annotFreeOfBinding :: Ord n => Binding k p n a -> Binding k p n (Free n a)
annotFreeOfBinding = \case
  Lambda a ns tm0 ->
    let
      tm =
        annotFreeOfTerm tm0

      free =
        freeVars (annotOfTerm tm) `Set.difference`
        Set.fromList ns
    in
      Lambda (Free free a) ns tm

annotFreeOfTail :: Ord n => Tail p n a -> Tail p n (Free n a)
annotFreeOfTail = \case
  Copy a xs0 ->
    let
      xs =
        fmap annotFreeOfAtom xs0

      free =
        Set.unions $
        fmap (freeVars . annotOfAtom) xs
    in
      Copy (Free free a) xs

  Call a n xs0 ->
    let
      xs =
        fmap annotFreeOfAtom xs0

      free =
        Set.unions $
        fmap (freeVars . annotOfAtom) xs
    in
      Call (Free free a) n xs

  Prim a p xs0 ->
    let
      xs =
        fmap annotFreeOfAtom xs0

      free =
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
