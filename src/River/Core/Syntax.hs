{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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


data Program a n =
    Program !a !(Term a n)
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, NFData)

data Term a n =
    Let !a ![n] !(Tail a n) !(Term a n)
  | Return !a !(Tail a n)
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, NFData)

data Tail a n =
    Copy !a ![Atom a n]
  | Unary !a !UnaryOp !(Atom a n)
  | Binary !a !BinaryOp !(Atom a n) !(Atom a n)
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, NFData)

data Atom a n =
    Immediate !a !Integer
  | Variable !a !n
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, NFData)

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
