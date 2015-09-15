module River.Source.Syntax where

import Data.Text (Text)

------------------------------------------------------------------------

data Program a = Program !a ![Statement a]
  deriving (Eq, Ord, Read, Show)

data Statement a =
    Declaration !a !(Identifier a)            !(Maybe (Expression a))
  | Assignment  !a !(Identifier a) !(Maybe BinaryOp) !(Expression a)
  | Return      !a                                   !(Expression a)
  deriving (Eq, Ord, Read, Show)

data Expression a =
    Literal  !a !Integer
  | Variable !a !(Identifier a)
  | Binary   !a !BinaryOp !(Expression a) !(Expression a)
  | Unary    !a !UnaryOp  !(Expression a)
  deriving (Eq, Ord, Read, Show)

data Identifier a = Identifier !a Text
  deriving (Eq, Ord, Read, Show)

data UnaryOp = Neg
  deriving (Eq, Ord, Read, Show)

data BinaryOp = Add | Sub | Mul | Div | Mod
  deriving (Eq, Ord, Read, Show)

------------------------------------------------------------------------

instance Functor Program where
  fmap f (Program a ss) = Program (f a) (fmap (fmap f) ss)

instance Functor Statement where
  fmap f (Declaration a ii    xx) = Declaration (f a) (fmap f ii)    (fmap (fmap f) xx)
  fmap f (Assignment  a ii op xx) = Assignment  (f a) (fmap f ii) op (fmap f xx)
  fmap f (Return      a       xx) = Return      (f a)                (fmap f xx)

instance Functor Expression where
  fmap f (Literal  a ii)       = Literal  (f a) ii
  fmap f (Variable a ii)       = Variable (f a) (fmap f ii)
  fmap f (Binary   a op x1 x2) = Binary   (f a) op (fmap f x1) (fmap f x2)
  fmap f (Unary    a op xx)    = Unary    (f a) op (fmap f xx)

instance Functor Identifier where
  fmap f (Identifier a tt)     = Identifier (f a) tt
