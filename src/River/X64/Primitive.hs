{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module River.X64.Primitive (
    Prim(..)
  ) where

import           Control.DeepSeq (NFData)

import           Data.Data (Data)
import           Data.Typeable (Typeable)

import           GHC.Generics (Generic)


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
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, NFData)
