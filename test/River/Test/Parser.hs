{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module River.Test.Parser (tests) where

import River.Source.Parser
import River.Source.Pretty
import River.Source.Reannotate
import River.Source.Syntax
import River.Test.Arbitraries

import Test.QuickCheck
import Test.QuickCheck.Property (succeeded, failed)

------------------------------------------------------------------------

prop_roundtrip (program :: Program X) =
      counterexample "\nRoundtrip:"
    . counterexample (either show show program')
    . counterexample "\nPretty Original:"
    . counterexample (displayProgram program)
    . counterexample "\nPretty Roundtrip:"
    . counterexample (either show displayProgram program')
    $ case program' of
        Left _                 -> failed
        Right p | program == p -> succeeded
                | otherwise    -> failed
  where
    source   = show (ppProgram program)
    program' = fmap stripAnnot (parseProgramFromString "qc" source)

    stripAnnot = reannotateProgram (const X)

------------------------------------------------------------------------

return []
tests :: IO Bool
tests = $quickCheckAll
