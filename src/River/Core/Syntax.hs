{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
module River.Core.Syntax (
    Program(..)
  , Term(..)
  , Tail(..)
  , Atom(..)
  , UnaryOp(..)
  , BinaryOp(..)
  ) where

import           Control.DeepSeq (NFData)

import           Data.Data (Data)
import           Data.Typeable (Typeable)

import           GHC.Generics (Generic)


data Program n a =
    Program !a !(Term n a)
    deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Data, Typeable, Generic, NFData)

data Term n a =
    Let !a ![n] !(Tail n a) !(Term n a)
  | Return !a !(Tail n a)
    deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Data, Typeable, Generic, NFData)

data Tail n a =
    Copy !a ![Atom n a]
  | Unary !a !UnaryOp !(Atom n a)
  | Binary !a !BinaryOp !(Atom n a) !(Atom n a)
    deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Data, Typeable, Generic, NFData)

data Atom n a =
    Immediate !a !Integer
  | Variable !a !n
    deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Data, Typeable, Generic, NFData)

data UnaryOp =
    Neg
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, NFData)

data BinaryOp =
    Add
  | Sub
  | Mul
  | Div
  | Mod
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, NFData)
