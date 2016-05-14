{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.River.Parser where

import River.Source.Parser
import River.Source.Pretty
import River.Source.Reannotate
import River.Source.Syntax

import Test.River.Arbitrary

import Test.QuickCheck
import Test.QuickCheck.Property (succeeded, failed)


prop_roundtrip (program :: Program X) =
  let
    source =
      show $ ppProgram program

    program' =
      fmap (reannotateProgram $ const X) $
      parseProgram' "qc" source
  in
    counterexample "\nRoundtrip:" .
    counterexample (either show show program') .
    counterexample "\nPretty Original:" .
    counterexample (displayProgram program) .
    counterexample "\nPretty Roundtrip:" .
    counterexample (either show displayProgram program') $
      case program' of
        Left _ ->
          failed
        Right p ->
          if program == p then
            succeeded
          else
            failed

return []
tests :: IO Bool
tests =
  $forAllProperties $
    quickCheckWithResult stdArgs { maxSuccess = 10000 }
