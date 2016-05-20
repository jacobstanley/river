{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
module River.Core.Syntax (
    Program(..)
  , Term(..)
  , Tail(..)
  , Atom(..)
  , Prim(..)
  ) where

import           Control.DeepSeq (NFData)

import           Data.Data (Data)
import           Data.Typeable (Typeable)
import           Data.Bifunctor.TH (deriveBifunctor, deriveBifoldable, deriveBitraversable)

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
  | Prim !a !Prim ![Atom n a]
    deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Data, Typeable, Generic, NFData)

data Atom n a =
    Immediate !a !Integer
  | Variable !a !n
    deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Data, Typeable, Generic, NFData)

data Prim =
    Neg
  | Add
  | Sub
  | Mul
  | DivMod
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, NFData)

$(deriveBifunctor ''Program)
$(deriveBifoldable ''Program)
$(deriveBitraversable ''Program)
$(deriveBifunctor ''Term)
$(deriveBifoldable ''Term)
$(deriveBitraversable ''Term)
$(deriveBifunctor ''Tail)
$(deriveBifoldable ''Tail)
$(deriveBitraversable ''Tail)
$(deriveBifunctor ''Atom)
$(deriveBifoldable ''Atom)
$(deriveBitraversable ''Atom)
