{-# LANGUAGE DeriveDataTypeable #-}

module River.Source.Syntax where

import Data.Data (Data)
import Data.Text (Text)
import Data.Typeable (Typeable)

------------------------------------------------------------------------

data Program a = Program !a !(Fragment a)
  deriving (Eq, Ord, Read, Show, Data, Typeable)

data Fragment a =
    Declaration !a !Identifier            !(Maybe (Expression a)) !(Fragment a)
  | Assignment  !a !Identifier !(Maybe BinaryOp) !(Expression a)  !(Fragment a)
  | Return      !a                               !(Expression a)
  deriving (Eq, Ord, Read, Show, Data, Typeable)

data Expression a =
    Literal  !a !Integer
  | Variable !a !Identifier
  | Unary    !a !UnaryOp  !(Expression a)
  | Binary   !a !BinaryOp !(Expression a) !(Expression a)
  deriving (Eq, Ord, Read, Show, Data, Typeable)

data Identifier = Identifier !Text
  deriving (Eq, Ord, Read, Show, Data, Typeable)

data UnaryOp = Neg
  deriving (Eq, Ord, Read, Show, Data, Typeable)

data BinaryOp = Add | Sub | Mul | Div | Mod
  deriving (Eq, Ord, Read, Show, Data, Typeable)

------------------------------------------------------------------------

instance Functor Program where
  fmap f (Program a ff) = Program (f a) (fmap f ff)

instance Functor Fragment where
  fmap f (Declaration a ii    xx ff) = Declaration (f a) ii    (fmap (fmap f) xx) (fmap f ff)
  fmap f (Assignment  a ii op xx ff) = Assignment  (f a) ii op (fmap f xx)        (fmap f ff)
  fmap f (Return      a       xx)    = Return      (f a)       (fmap f xx)

instance Functor Expression where
  fmap f (Literal  a ii)       = Literal  (f a) ii
  fmap f (Variable a ii)       = Variable (f a) ii
  fmap f (Unary    a op xx)    = Unary    (f a) op (fmap f xx)
  fmap f (Binary   a op x1 x2) = Binary   (f a) op (fmap f x1) (fmap f x2)
