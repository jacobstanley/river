{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module River.Name (
    Name(..)
  ) where

import           Control.DeepSeq (NFData)

import           Data.Data (Data)
import           Data.Typeable (Typeable)

import           GHC.Generics (Generic)


data Name n =
    Name !n
  | NameMod !n !Int
  | NameNew !Int
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, NFData)
