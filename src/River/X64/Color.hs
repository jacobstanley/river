{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module River.X64.Color (
    colorByRegister

  , RegisterError(..)
  ) where

import           Control.Monad.Trans.State.Strict (StateT, runStateT, get, put)

import           Data.Function (on)
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map

import           River.Core.Color (ColorStrategy(..))
import           River.Core.Syntax
import           River.Fresh
import           River.Map
import           River.X64.Primitive
import           River.X64.Syntax (Register64(..))


data RegisterError n =
    RegistersExhausted !n
    deriving (Eq, Ord, Show)

colorByRegister :: Ord n => ColorStrategy (RegisterError n) Register64 k Prim n a
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

precoloredOfProgram ::
  Ord n =>
  FreshName n =>
  Program k Prim n a ->
  Fresh (Map n Register64, Program k Prim n a)
precoloredOfProgram = \case
  Program a tm0 -> do
    (tm, rs) <- runStateT (precoloredOfTerm tm0) Map.empty
    pure (rs, Program a tm)

putsert :: (Ord k, Monad m) => k -> v -> StateT (Map k v) m ()
putsert k v = do
  kvs <- get
  put $ Map.insert k v kvs

precoloredOfTerm ::
  Ord n =>
  FreshName n =>
  Term k Prim n a ->
  StateT (Map n Register64) Fresh (Term k Prim n a)
precoloredOfTerm = \case
  -- TODO ensure in RAX
  Return at tl ->
    pure $
      Return at tl

  If at k i t0 e0 ->
    If at k i
      <$> precoloredOfTerm t0
      <*> precoloredOfTerm e0

  LetRec at bs tm -> do
    LetRec at
      <$> precoloredOfBindings bs
      <*> precoloredOfTerm tm

  Let at [lo, hi] (Prim ap Imul [Variable ax x, y]) tm0 -> do
    tm <- precoloredOfTerm tm0

    x_rax <- freshen x
    lo_rax <- freshen lo
    hi_rdx <- freshen hi

    putsert x_rax RAX
    putsert lo_rax RAX
    putsert hi_rdx RDX

    pure $
      Let ap [x_rax]          (Copy ap [Variable ax x]) $
      Let at [lo_rax, hi_rdx] (Prim ap Imul [Variable ax x_rax, y]) $
      Let at [lo]             (Copy at [Variable at lo_rax]) $
      Let at [hi]             (Copy at [Variable at hi_rdx]) $
      tm

  Let at [dv, md] (Prim ap Idiv [Variable al lo, Variable ah hi, x]) tm0 -> do
    tm <- precoloredOfTerm tm0

    lo_rax <- freshen lo
    hi_rdx <- freshen hi
    dv_rax <- freshen dv
    md_rdx <- freshen md

    putsert lo_rax RAX
    putsert hi_rdx RDX
    putsert dv_rax RAX
    putsert md_rdx RDX

    pure $
      Let ap [lo_rax]         (Copy ap [Variable al lo]) $
      Let ap [hi_rdx]         (Copy ap [Variable ah hi]) $
      Let at [dv_rax, md_rdx] (Prim ap Idiv [Variable al lo_rax, Variable ah hi_rdx, x]) $
      Let at [dv]             (Copy at [Variable at dv_rax]) $
      Let at [md]             (Copy at [Variable at md_rdx]) $
      tm

  Let at [hi] (Prim ap Cqto [Variable al lo]) tm0 -> do
    tm <- precoloredOfTerm tm0

    lo_rax <- freshen lo
    hi_rdx <- freshen hi

    putsert lo_rax RAX
    putsert hi_rdx RDX

    pure $
      Let ap [lo_rax] (Copy ap [Variable al lo]) $
      Let at [hi_rdx] (Prim ap Cqto [Variable al lo_rax]) $
      Let at [hi]     (Copy at [Variable at hi_rdx]) $
      tm

  Let at ns tl tm ->
    Let at ns tl
      <$> precoloredOfTerm tm

precoloredOfBindings ::
  Ord n =>
  FreshName n =>
  Bindings k Prim n a ->
  StateT (Map n Register64) Fresh (Bindings k Prim n a)
precoloredOfBindings = \case
  Bindings a nbs0 -> do
    let
      (ns, bs0) =
        unzip nbs0
    bs <- traverse precoloredOfBinding bs0
    pure $
      Bindings a (zip ns bs)

precoloredOfBinding ::
  Ord n =>
  FreshName n =>
  Binding k Prim n a ->
  StateT (Map n Register64) Fresh (Binding k Prim n a)
precoloredOfBinding = \case
  Lambda a ns tm -> do
    Lambda a ns
      <$> precoloredOfTerm tm

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
