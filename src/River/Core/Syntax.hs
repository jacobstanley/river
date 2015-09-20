{-# LANGUAGE DeriveDataTypeable #-}

module River.Core.Syntax where

import Data.Data (Data)
import Data.Typeable (Typeable)

------------------------------------------------------------------------

data Program a n = Program !a !(Term a n)
  deriving (Eq, Ord, Read, Show, Data, Typeable)

data Term a n =
    Let    !a ![n] !(Tail a n) !(Term a n)
  | Return !a      !(Tail a n)
  deriving (Eq, Ord, Read, Show, Data, Typeable)

data Tail a n =
    Copy   !a           ![Atom a n]
  | Unary  !a !UnaryOp  !(Atom a n)
  | Binary !a !BinaryOp !(Atom a n) !(Atom a n)
  deriving (Eq, Ord, Read, Show, Data, Typeable)

data Atom a n =
    Immediate !a !Integer
  | Variable  !a !n
  deriving (Eq, Ord, Read, Show, Data, Typeable)

data UnaryOp = Neg
  deriving (Eq, Ord, Read, Show, Data, Typeable)

data BinaryOp = Add | Sub | Mul | Div | Mod
  deriving (Eq, Ord, Read, Show, Data, Typeable)
