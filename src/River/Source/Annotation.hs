{-# LANGUAGE LambdaCase #-}
module River.Source.Annotation (
    annotOfProgram
  , annotOfBlock
  , annotOfStatement
  , annotOfExpression
  ) where

import           River.Source.Syntax


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
  Declare a _ _ _ ->
    a
  Assign a _ _ ->
    a
  If a _ _ _ ->
    a
  While a _ _ ->
    a
  Return a _ ->
    a

annotOfExpression :: Expression a -> a
annotOfExpression = \case
  Literal a _ ->
    a
  Variable a _ ->
    a
  Unary a _ _ ->
    a
  Binary a _ _ _ ->
    a
  Conditional a _ _ _ ->
    a
