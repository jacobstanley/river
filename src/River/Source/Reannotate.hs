{-# LANGUAGE LambdaCase #-}

module River.Source.Reannotate where

import River.Source.Syntax

------------------------------------------------------------------------

reannotateProgram :: (a -> b) -> Program a -> Program b
reannotateProgram f = \case
  Program a ss -> Program (f a) (fmap (reannotateStatement f) ss)

reannotateStatement :: (a -> b) -> Statement a -> Statement b
reannotateStatement f = \case
  Declaration a ii    xx -> Declaration (f a) ii    (fmap (reannotateExpression f) xx)
  Assignment  a ii op xx -> Assignment  (f a) ii op (reannotateExpression f xx)
  Return      a       xx -> Return      (f a)       (reannotateExpression f xx)

reannotateExpression :: (a -> b) -> Expression a -> Expression b
reannotateExpression f = \case
  Literal  a ii       -> Literal  (f a) ii
  Variable a ii       -> Variable (f a) ii
  Unary    a op xx    -> Unary    (f a) op (reannotateExpression f xx)
  Binary   a op xx yy -> Binary   (f a) op (reannotateExpression f xx)
                                           (reannotateExpression f yy)
