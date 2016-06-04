{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
module River.Core.Analysis.Live (
    Live(..)
  , LiveError(..)

  , liveOfProgram
  , liveOfTerm
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import           River.Bifunctor
import           River.Core.Analysis.Free
import           River.Core.Annotation
import           River.Core.Syntax


data LiveError n =
    LambdaBindingNotFound !n !(Map n (Set n))
    deriving (Eq, Ord, Show)

data Live n a =
  Live {
      liveVars :: !(Set n)
    , liveTail :: !a
    } deriving (Eq, Ord, Show, Functor)

fromFree :: Free n a -> Live n a
fromFree = \case
  Free vars tl ->
    Live vars tl

liveOfProgram :: Ord n => Program k p n a -> Either (LiveError n) (Program k p n (Live n a))
liveOfProgram = \case
  Program a0 tm0 -> do
    tm <- liveOfTerm tm0

    let
      a =
        a0 <$ annotOfTerm tm

    pure $
      Program a tm

liveOfTerm :: Ord n => Term k p n a -> Either (LiveError n) (Term k p n (Live n a))
liveOfTerm =
  liveOfTerm' Map.empty . annotFreeOfTerm

liveOfTerm' ::
  Ord n =>
  Map n (Set n) ->
  Term k p n (Free n a) ->
  Either (LiveError n) (Term k p n (Live n a))
liveOfTerm' lam_fvs0 = \case
  Return (Free _ a) tl0 -> do
    tl <- liveOfTail lam_fvs0 tl0

    let
      live =
        liveVars $ annotOfTail tl

    pure $
      Return (Live live a) tl

  If (Free _ a) k i0 t0 e0 -> do
    t <- liveOfTerm' lam_fvs0 t0
    e <- liveOfTerm' lam_fvs0 e0

    let
      i =
        liveOfAtom i0

      live =
        liveVars (annotOfAtom i) `Set.union`
        liveVars (annotOfTerm t) `Set.union`
        liveVars (annotOfTerm e)

    pure $
      If (Live live a) k i t e

  Let (Free _ a) ns tl0 tm0 -> do
    tl <- liveOfTail lam_fvs0 tl0
    tm <- liveOfTerm' lam_fvs0 tm0

    let
      live =
        Set.union
          (liveVars (annotOfTail tl))
          (liveVars (annotOfTerm tm) `Set.difference` Set.fromList ns)

    pure $
      Let (Live live a) ns tl tm

  LetRec (Free _ a) bs0 tm0 -> do
    (lam_fvs, bs) <- liveOfBindings lam_fvs0 bs0
    tm <- liveOfTerm' lam_fvs tm0

    let
      live =
        Set.difference
          (liveVars (annotOfBindings bs) `Set.union` liveVars (annotOfTerm tm))
          (boundOfBindings bs)

    pure $
      LetRec (Live live a) bs tm

liveOfBindings ::
  Ord n =>
  Map n (Set n) ->
  Bindings k p n (Free n a) ->
  Either
    (LiveError n)
    (Map n (Set n), Bindings k p n (Live n a))
liveOfBindings lam_fvs0 = \case
  Bindings (Free _ a) bs0 -> do
    let
      lam_fvs1 =
        Map.fromList $ fmap (second $ freeVars . annotOfBinding) bs0

      lam_fvs =
        lam_fvs1 `Map.union`
        lam_fvs0

    bs <- traverse (secondA $ liveOfBinding lam_fvs) bs0

    let
      live =
        Set.unions $ fmap (liveVars . annotOfBinding . snd) bs

    pure $
      (lam_fvs, Bindings (Live live a) bs)

liveOfBinding ::
  Ord n =>
  Map n (Set n) ->
  Binding k p n (Free n a) ->
  Either (LiveError n) (Binding k p n (Live n a))
liveOfBinding lam_fvs = \case
  Lambda (Free _ a) ns tm0 -> do
    tm <- liveOfTerm' lam_fvs tm0

    let
      live =
        liveVars (annotOfTerm tm) `Set.difference`
        Set.fromList ns

    pure $
      Lambda (Live live a) ns tm

liveOfTail ::
  Ord n =>
  Map n (Set n) ->
  Tail p n (Free n a) ->
  Either (LiveError n) (Tail p n (Live n a))
liveOfTail lam_fvs = \case
  Copy a xs ->
    pure $
      Copy (fromFree a) (fmap liveOfAtom xs)

  Call (Free fvs0 a) n xs ->
    case Map.lookup n lam_fvs of
      Nothing ->
        Left $ LambdaBindingNotFound n lam_fvs
      Just fvs -> do
        let
          live =
            fvs `Set.union` fvs0

        pure $
          Call (Live live a) n (fmap liveOfAtom xs)

  Prim a p xs ->
    pure $
      Prim (fromFree a) p (fmap liveOfAtom xs)

liveOfAtom :: Atom n (Free n a) -> Atom n (Live n a)
liveOfAtom = \case
  Immediate a i ->
    Immediate (fromFree a) i
  Variable a n ->
    Variable (fromFree a) n
