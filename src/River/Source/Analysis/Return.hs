{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
module River.Source.Analysis.Return (
    Return(..)

  , returnOfProgram
  , returnOfBlock
  , returnOfStatement
  ) where

import           River.Source.Syntax


data Return a =
    ReturnOK
  | ReturnKO !a
    deriving (Eq, Ord, Show, Functor)

ensureAnnot :: a -> Return (Maybe a) -> Return a
ensureAnnot x = \case
  ReturnOK ->
    ReturnOK
  ReturnKO Nothing ->
    ReturnKO x
  ReturnKO (Just y) ->
    ReturnKO y

returnOfProgram :: Program a -> Return a
returnOfProgram = \case
  Program a ss ->
    ensureAnnot a $
    returnOfBlock ss

returnOfBlock :: Block a -> Return (Maybe a)
returnOfBlock = \case
  Block _ [] ->
    ReturnKO Nothing
  Block a (s : ss) ->
    case returnOfStatement s of
      ReturnOK ->
        ReturnOK
      ReturnKO _ ->
        returnOfBlock (Block a ss)

returnOfStatement :: Statement a -> Return a
returnOfStatement = \case
  Declare a _ _ ss ->
    ensureAnnot a $
    returnOfBlock ss

  Assign a _ _ ->
    ReturnKO a

  If a _ t e ->
    ensureAnnot a $
    case returnOfBlock t of
      ReturnOK ->
        returnOfBlock e
      ko ->
        ko

  Return _ _ ->
    ReturnOK
