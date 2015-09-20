{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad.Trans.Either (runEitherT)

import           Data.Monoid ((<>))
import qualified Data.List as List
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified River.Core.Pretty as Core

import           River.Source.Check
import           River.Source.Parser
import qualified River.Source.Pretty as Source
import           River.Source.Reannotate
import           River.Source.Syntax
import           River.Source.ToCore

import           System.Environment (getArgs)

import           Text.Trifecta.Delta (Delta)

------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    mapM_ go args
  where
    go arg = do
        putStrLn arg
        putStrLn (take (length arg) (repeat '='))

        p <- runEitherT (parseProgram arg)
        case p of
          Left (TrifectaError xx) -> print xx
          Right program           -> do
            putStrLn (Source.displayProgram program)

            let errs = List.sort
                     . concatMap ppCheckError
                     . checkProgram
                     . reannotateProgram locationOfDelta
                     $ program
            mapM_ (T.putStrLn . ppError) errs

            let core = coreOfProgram program
            putStrLn (Core.displayProgram core)
        putStrLn ""

------------------------------------------------------------------------

data Location = Location FilePath Int Int
  deriving (Eq, Ord, Read, Show)

locationOfDelta :: Delta -> Location
locationOfDelta d = Location (fileOfDelta d) (lineOfDelta d) (columnOfDelta d)

ppError :: (Location, Text) -> Text
ppError (loc, msg) = ppLocation loc <> ": error: " <> msg

ppLocation :: Location -> Text
ppLocation (Location file line column) =
    T.pack file <> ":" <> T.pack (show line)
                <> ":" <> T.pack (show column)

ppCheckError :: CheckError Location -> [(Location, Text)]
ppCheckError = \case
    UndeclaredVariable    n locs -> zip (Set.toList locs) (cycle [ppUndecl n])
    UninitializedVariable n locs -> zip (Set.toList locs) (cycle [ppUninit n])
    NoReturnStatement       loc  -> [(loc, ppNoRet)]
  where
    ppUndecl (Identifier n) = "undeclared variable '" <> n <> "'"
    ppUninit (Identifier n) = "uninitialized variable '" <> n <> "'"
    ppNoRet                 = "return statement not found"
