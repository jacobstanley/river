{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module River.X64.Color (
    colorByRegister

  , RegisterError(..)
  ) where

import           Data.Function (on)
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)

import           River.Core.Color (ColorStrategy(..))
import           River.Core.Syntax
import           River.Fresh
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

precoloredOfProgram ::
  Ord n =>
  FreshName n =>
  Program Prim n a ->
  Fresh (Map n Register64, Program Prim n a)
precoloredOfProgram = \case
  Program a tm ->
    (fmap . fmap) (Program a) (precoloredOfTerm tm)

precoloredOfTerm ::
  Ord n =>
  FreshName n =>
  Term Prim n a ->
  Fresh (Map n Register64, Term Prim n a)
precoloredOfTerm = \case
  -- TODO Should probably add extra let bindings for mul/div so that rax/rdx
  -- TODO are free for use again.

  Let at [lo, hi] (Prim ap Imul [Variable ax x, y]) tm0 -> do
    (colors0, tm) <- precoloredOfTerm tm0

    x_rax <- freshen x
    lo_rax <- freshen lo
    hi_rdx <- freshen hi

    let
      colors =
        Map.unions
          [ Map.singleton x_rax RAX
          , Map.singleton lo_rax RAX
          , Map.singleton hi_rdx RDX
          , colors0 ]

    pure . (colors,) $
      Let ap [x_rax]          (Copy ap [Variable ax x]) $
      Let at [lo_rax, hi_rdx] (Prim ap Imul [Variable ax x_rax, y]) $
      Let at [lo]             (Copy at [Variable at lo_rax]) $
      Let at [hi]             (Copy at [Variable at hi_rdx]) $
      tm

  Let at [dv, md] (Prim ap Idiv [Variable al lo, Variable ah hi, x]) tm0 -> do
    (colors0, tm) <- precoloredOfTerm tm0

    lo_rax <- freshen lo
    hi_rdx <- freshen hi
    dv_rax <- freshen dv
    md_rdx <- freshen md

    let
      colors =
        Map.unions
          [ Map.singleton lo_rax RAX
          , Map.singleton hi_rdx RDX
          , Map.singleton dv_rax RAX
          , Map.singleton md_rdx RDX
          , colors0 ]

    pure . (colors,) $
      Let ap [lo_rax]         (Copy ap [Variable al lo]) $
      Let ap [hi_rdx]         (Copy ap [Variable ah hi]) $
      Let at [dv_rax, md_rdx] (Prim ap Idiv [Variable al lo_rax, Variable ah hi_rdx, x]) $
      Let at [dv]             (Copy at [Variable at dv_rax]) $
      Let at [md]             (Copy at [Variable at md_rdx]) $
      tm

  Let at [hi] (Prim ap Cqto [Variable al lo]) tm0 -> do
    (colors0, tm) <- precoloredOfTerm tm0

    lo_rax <- freshen lo
    hi_rdx <- freshen hi

    let
      colors =
        Map.unions
          [ Map.singleton lo_rax RAX
          , Map.singleton hi_rdx RDX
          , colors0 ]

    pure . (colors,) $
      Let ap [lo_rax] (Copy ap [Variable al lo]) $
      Let at [hi_rdx] (Prim ap Cqto [Variable al lo_rax]) $
      Let at [hi]     (Copy at [Variable at hi_rdx]) $
      tm

  Let at ns tl tm ->
    (fmap . fmap) (Let at ns tl) (precoloredOfTerm tm)

  -- TODO ensure in RAX
  Return at tl ->
    pure (Map.empty, Return at tl)

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
