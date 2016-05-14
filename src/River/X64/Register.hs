{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module River.X64.Register (
    General(..)
  ) where

import           Control.DeepSeq (NFData)

import           Data.Data (Data)
import           Data.Typeable (Typeable)

import           GHC.Generics (Generic)

data General =
    RAX
  | RBX
  | RCX
  | RDX
  | RBP
  | RSP
  | RSI
  | RDI
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, NFData)
