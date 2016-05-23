{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
module River.Source.Syntax (
    Program(..)
  , Block(..)
  , Statement(..)
  , Expression(..)
  , Literal(..)
  , Identifier(..)
  , Type(..)
  , UnaryOp(..)
  , BinaryOp(..)
  ) where

import           Control.DeepSeq (NFData)

import           Data.Data (Data)
import           Data.Text (Text)
import           Data.Typeable (Typeable)

import           GHC.Generics (Generic)


data Program a =
    Program !a !(Block a)
    deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Data, Typeable, Generic, NFData)

data Block a =
    Block !a ![Statement a]
    deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Data, Typeable, Generic, NFData)

data Statement a =
    Declare !a !Type !Identifier !(Block a)
  | Assign !a !Identifier !(Expression a)
  | If !a !(Expression a) !(Block a) !(Block a)
  | Return !a !(Expression a)
    deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Data, Typeable, Generic, NFData)

data Expression a =
    Literal !a !Literal
  | Variable !a !Identifier
  | Unary !a !UnaryOp !(Expression a)
  | Binary !a !BinaryOp !(Expression a) !(Expression a)
  | Conditional !a !(Expression a) !(Expression a) !(Expression a)
    deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Data, Typeable, Generic, NFData)

data Literal =
    LiteralInt !Integer
  | LiteralBool !Bool
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, NFData)

data Identifier =
    Identifier !Text
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, NFData)

data Type =
    Int
  | Bool
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, NFData)

data UnaryOp =
    LNot
  | BNot
  | Neg
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, NFData)

data PostOp =
    Inc
  | Dec
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, NFData)

data BinaryOp =
    Mul
  | Div
  | Mod
  | Add
  | Sub
  | Shl
  | Shr
  | Lt
  | Le
  | Gt
  | Ge
  | Eq
  | NEq
  | BAnd
  | BXor
  | BOr
  | LAnd
  | LOr
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, NFData)
