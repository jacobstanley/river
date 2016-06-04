{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module River.X64.Primitive (
    Prim(..)
  , Cc(..)
  ) where

import           Control.DeepSeq (NFData)

import           Data.Data (Data)
import           Data.Typeable (Typeable)

import           GHC.Generics (Generic)

import           River.X64.Syntax (Cc(..))


data Prim =
    Neg
  | Not

  | Add
  | Sub
  | Imul
  | Idiv
  | Cqto

  | And
  | Xor
  | Or

  | Sal
  | Sar

  | Movzbq

  | Test
  | Cmp
  | Set !Cc
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, NFData)
