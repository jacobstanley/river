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
import           River.X64.Primitive
import           River.X64.Syntax (Register64(..))


data RegisterError n =
    RegistersExhausted !n
    deriving (Eq, Ord, Show)

colorByRegister :: Ord n => ColorStrategy (RegisterError n) Register64 Prim n a
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

precoloredOfProgram :: Ord n => Program Prim n a -> Map n Register64
precoloredOfProgram = \case
  Program _ tm ->
    precoloredOfTerm tm

precoloredOfTerm :: Ord n => Term Prim n a -> Map n Register64
precoloredOfTerm = \case
  -- TODO Should probably add extra let bindings for mul/div so that rax/rdx
  -- TODO are free for use again.

  Let _ [dst0, dst1] (Prim _ Imul [Variable _ x, _]) tm ->
    Map.unions
      [ Map.singleton dst0 RAX
      , Map.singleton dst1 RDX
      , Map.singleton x RAX
      , precoloredOfTerm tm ]

  Let _ [dst0, dst1] (Prim _ Idiv [Variable _ high, Variable _ low, _]) tm ->
    Map.unions
      [ Map.singleton dst0 RAX
      , Map.singleton dst1 RDX
      , Map.singleton high RDX
      , Map.singleton low RAX
      , precoloredOfTerm tm ]

  Let _ [high] (Prim _ Cqto [Variable _ low]) tm ->
    Map.unions
      [ Map.singleton high RDX
      , Map.singleton low RAX
      , precoloredOfTerm tm ]

  Let _ _ _ tm ->
    precoloredOfTerm tm

  -- TODO ensure in RAX
  Return _ _ ->
    Map.empty

registers :: Set Register64
registers =
  Set.fromList

  --
  -- Caller saved registers
  --
  --   We can overwrite these at will.
  --
    [ RAX
    , RCX
    , RDX
    , RSI
    , RDI
    , R8
    , R9
    , R10

 --
 -- Spill register
 --
 --   We will use this to spill and restore from the stack.
 --
 -- , R11
 --

 --
 -- Stack pointer
 --
 -- , RSP
 --

 --
 -- Callee saved registers
 --
 --   If we use these, they must be saved to the stack and restored before
 --   returning.
 --
    , R12
    , R13
    , R14
    , R15
    , RBX
 -- , RBP
    ]
