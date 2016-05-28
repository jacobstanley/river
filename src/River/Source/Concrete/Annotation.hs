{-# LANGUAGE LambdaCase #-}
module River.Source.Concrete.Annotation (
    annotOfProgram
  , annotOfBlock
  , annotOfStatement
  , annotOfSimple
  , annotOfControl
  , annotOfLValue
  ) where

import           River.Source.Concrete.Syntax


annotOfProgram :: Program a -> a
annotOfProgram = \case
  Program a _ ->
    a

annotOfBlock :: Block a -> a
annotOfBlock = \case
  Block a _ ->
    a

annotOfStatement :: Statement a -> a
annotOfStatement = \case
  SSimple a _ ->
    a
  SControl a _ ->
    a
  SBlock a _ ->
    a

annotOfSimple :: Simple a -> a
annotOfSimple = \case
  Assign a _ _ _ ->
    a
  Post a _ _ ->
    a
  Declare a _ _ _ ->
    a

annotOfControl :: Control a -> a
annotOfControl = \case
  If a _ _ _ ->
    a
  While a _ _ ->
    a
  For a _ _ _ _ ->
    a
  Return a _ ->
    a

annotOfLValue :: LValue a -> a
annotOfLValue = \case
  LIdentifier a _ ->
    a
