{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module River.X64.Name (
    Name(..)
  ) where

import           Control.DeepSeq (NFData)

import           Data.Data (Data)
import           Data.Typeable (Typeable)

import           GHC.Generics (Generic)

import           River.X64.Syntax


data Name =
    Lb !Label
  | Rg !Register64
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, NFData)
