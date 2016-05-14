{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module River.Source.Syntax (
    Program(..)
  , Statement(..)
  , Expression(..)
  , Identifier(..)
  , UnaryOp(..)
  , BinaryOp(..)
  ) where

import           Control.DeepSeq (NFData)

import           Data.Data (Data)
import           Data.Text (Text)
import           Data.Typeable (Typeable)

import           GHC.Generics (Generic)


data Program a =
    Program !a ![Statement a]
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, NFData)

data Statement a =
    Declaration !a !Identifier !(Maybe (Expression a))
  | Assignment !a !Identifier !(Maybe BinaryOp) !(Expression a)
  | Return !a !(Expression a)
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, NFData)

data Expression a =
    Literal !a !Integer
  | Variable !a !Identifier
  | Unary !a !UnaryOp !(Expression a)
  | Binary !a !BinaryOp !(Expression a) !(Expression a)
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, NFData)

data Identifier =
    Identifier !Text
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
