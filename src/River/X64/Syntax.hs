{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module River.X64.Syntax (
    Instruction(..)
  , Cc(..)
  , Operand64(..)
  , Register64(..)
  , Label(..)
  ) where

import           Control.DeepSeq (NFData)

import           Data.Text (Text)
import           Data.Data (Data)
import           Data.Typeable (Typeable)
import           Data.Word (Word64)

import           GHC.Generics (Generic)

import           River.Name


data Instruction =
    Movq !Operand64 !Operand64
  | Negq !Operand64
  | Addq !Operand64 !Operand64
  | Subq !Operand64 !Operand64
  | Imulq !Operand64
  | Cqto
  | Idivq !Operand64
  | Movzbq !Operand64 !Operand64
  | Lahf
  | Sahf
  | Cmpq !Operand64 !Operand64
  | Test !Operand64 !Operand64
  | Set !Cc !Operand64
  | J !Cc !Label
  | Jmp !Label
  | Lbl !Label
  | Ret
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, NFData)

data Cc =
    Z  -- ^ if zero
  | E  -- ^ if equal
  | Nz -- ^ if not zero
  | Ne -- ^ if not equal
  | L  -- ^ if less than
  | Le -- ^ if less or equal
  | G  -- ^ if greater than
  | Ge -- ^ if greater or equal
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, NFData)

data Operand64 =
    Register64 !Register64
  | Immediate64 !Word64
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, NFData)

newtype Label =
  Label {
      unLabel :: Name Text
    } deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, NFData)

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
  | RFLAGS
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, NFData)
