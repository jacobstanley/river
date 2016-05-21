{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module River.X64.Syntax (
    Instruction(..)
  , Operand64(..)
  , Register64(..)
  ) where

import           Control.DeepSeq (NFData)

import           Data.Data (Data)
import           Data.Typeable (Typeable)
import           Data.Word (Word64)

import           GHC.Generics (Generic)


data Instruction =
    Movq !Operand64 !Operand64
  | Negq !Operand64
  | Addq !Operand64 !Operand64
  | Subq !Operand64 !Operand64
  | Imulq !Operand64
  | Cqto
  | Idivq !Operand64
  | Ret
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, NFData)

data Operand64 =
    Register64 !Register64
  | Immediate64 !Word64
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, NFData)

data Register64 =
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
