{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module River.X64.Color (
    colorByRegister

  , RegisterError(..)
  ) where

import           Data.Function (on)
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)

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
        \n used ->
          case Map.toList (registers `mapDifferenceSet` used) of
            [] ->
              Left $ RegistersExhausted n
            regs ->
              pure . fst $ List.minimumBy (compare `on` snd) regs

    , precolored =
        precoloredOfProgram
    }

mapDifferenceSet :: Ord k => Map k v -> Set k -> Map k v
mapDifferenceSet m =
  Map.difference m . Map.fromSet (const ())

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

registers :: Map Register64 Int
registers =
  Map.fromList $ flip zip [1..]

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
