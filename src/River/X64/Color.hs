{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module River.X64.Color (
    colorByRegister

  , RegisterError(..)
  ) where

import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map (Map)
import qualified Data.Map as Map

import           River.Core.Analysis.Bindings
import           River.Core.Color
import           River.Core.Syntax
import           River.X64.Syntax


data RegisterError n =
    RegistersExhausted !n
    deriving (Eq, Ord, Show)

colorByRegister :: Ord n => ColorStrategy (RegisterError n) Register64 n a
colorByRegister =
  ColorStrategy {
      unusedColor =
        \(Binding _ n _ _) used ->
          case Set.minView (registers `Set.difference` used) of
            Nothing ->
              Left $ RegistersExhausted n
            Just (reg, _) ->
              pure reg

    , precolored =
        precoloredOfProgram
    }

precoloredOfProgram :: Ord n => Program n a -> Map n Register64
precoloredOfProgram = \case
  Program _ tm ->
    precoloredOfTerm tm

precoloredOfTerm :: Ord n => Term n a -> Map n Register64
precoloredOfTerm = \case
  -- TODO This should probably do add extra let bindings so that rax/rdx are
  -- TODO free for use again.
  Let _ [dst0, dst1] (Prim _ DivMod [Variable _ x, _]) tm ->
    Map.unions
      [ Map.singleton dst0 RAX
      , Map.singleton dst1 RDX
      , Map.singleton x RAX
      , precoloredOfTerm tm ]

  Let _ _ _ tm ->
    precoloredOfTerm tm

  -- TODO ensure in RAX
  Return _ _ ->
    Map.empty

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
