{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module River.X64.Color (
    colorByRegister

  , RegisterError(..)
  ) where

import           Data.Set (Set)
import qualified Data.Set as Set

import           River.Core.Analysis.Bindings
import           River.Core.Color
import           River.X64.Syntax


data RegisterError n =
    RegistersExhausted !n
    deriving (Eq, Ord, Show)

colorByRegister :: ColorStrategy (RegisterError n) Register64 n a
colorByRegister =
  ColorStrategy $ \(Binding _ n _ _) used ->
    case Set.minView (registers `Set.difference` used) of
      Nothing ->
        Left $ RegistersExhausted n
      Just (reg, _) ->
        pure reg

registers :: Set Register64
registers =
  Set.fromList
    [ RAX
    , RBX
    , RCX
    , RDX
 -- , RBP
 -- , RSP
    , RSI
    , RDI
    , R8
    , R9
    , R10
    , R11
    , R12
    , R13
    , R14
    , R15
    ]
