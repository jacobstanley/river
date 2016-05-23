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
  , Bindings(..)
  , Binding(..)
  ) where

import           Control.DeepSeq (NFData)

import           Data.Bifunctor.TH (deriveBifunctor, deriveBifoldable, deriveBitraversable)
import           Data.Data (Data)
import           Data.Typeable (Typeable)

import           GHC.Generics (Generic)


data Program p n a =
    Program !a !(Term p n a)
    deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Data, Typeable, Generic, NFData)

data Term p n a =
    Return !a !(Tail p n a)
  | If !a !(Atom n a) !(Term p n a) !(Term p n a)
  | Let !a ![n] !(Tail p n a) !(Term p n a)
  | LetRec !a !(Bindings p n a) !(Term p n a)
    deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Data, Typeable, Generic, NFData)

data Tail p n a =
    Copy !a ![Atom n a]
  | Call !a !n ![Atom n a]
  | Prim !a !p ![Atom n a]
    deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Data, Typeable, Generic, NFData)

data Atom n a =
    Immediate !a !Integer
  | Variable !a !n
    deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Data, Typeable, Generic, NFData)

data Bindings p n a =
    Bindings !a [(n, Binding p n a)]
    deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Data, Typeable, Generic, NFData)

data Binding p n a =
    Lambda !a ![n] !(Term p n a)
    deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Data, Typeable, Generic, NFData)

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
$(deriveBifunctor ''Bindings)
$(deriveBifoldable ''Bindings)
$(deriveBitraversable ''Bindings)
$(deriveBifunctor ''Binding)
$(deriveBifoldable ''Binding)
$(deriveBitraversable ''Binding)
