{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module River.Core.Primitive (
    Prim(..)
  ) where

import           Control.DeepSeq (NFData)

import           Data.Data (Data)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable (Typeable)

import           GHC.Generics (Generic)

import           River.Effect


data Prim =
    Neg
  | Not

  | Add
  | Sub
  | Mul
  | Div
  | Mod

  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge

  | And
  | Xor
  | Or

  | Shl
  | Shr
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, NFData)

instance HasEffect Prim where
  hasEffect p =
    Set.member p effectfulPrims

effectfulPrims :: Set Prim
effectfulPrims =
  Set.fromList [Mul, Div, Mod]
