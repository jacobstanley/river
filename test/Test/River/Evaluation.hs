{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.River.Evaluation where

import           Control.Monad.Trans.Except (runExceptT)

import           River.Compile
import           River.Core.Evaluator
import qualified River.Core.Pretty as Core
import           River.Source.Check
import qualified River.Source.Pretty as Source
import           River.Source.Syntax
import           River.Source.ToCore
import           River.X64.FromCore
import qualified River.X64.Pretty as X64

import           System.IO.Unsafe (unsafePerformIO)

import           Test.River.Arbitrary

import           Test.QuickCheck
import           Test.QuickCheck.Property (succeeded, failed)


prop_compile (program :: Program X) =
  if not . null $ checkProgram program then
    discard
  else
    let
      source =
        show $ Source.ppProgram program

      core =
        coreOfProgram program

      core_result =
        evaluateProgram core

      easm =
        assemblyOfProgram core

      asm_result =
        unsafePerformIO . runExceptT $ executeBinary' source
    in
      counterexample "\n-- Source --" .
      counterexample (Source.displayProgram program) .
      counterexample "\n-- Core --\n" .
      counterexample (Core.displayProgram core) .
      counterexample "\n-- Assembly --\n" .
      counterexample (either show (X64.displayProgram X64.Color) easm) .
      counterexample "\n-- Core Eval --\n" .
      counterexample (show core_result) .
      counterexample "\n-- Assembly Eval --\n" .
      counterexample (show asm_result) $
        case (core_result, asm_result) of
          (Right [VInt64 x], Right (ExecuteResult y)) ->
            if x == y then
              succeeded
            else
              failed
          (Left _, Right (ExecuteError _ _ _)) ->
            succeeded
          _ ->
            failed

return []
tests :: IO Bool
tests =
  $forAllProperties $
    quickCheckWithResult stdArgs { maxSize = 60, maxSuccess = 500, maxDiscardRatio = 10000 }
