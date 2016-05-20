{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Trans.Except (runExceptT)

import           Data.Monoid ((<>))
import qualified Data.List as List
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           River.Core.Color
import qualified River.Core.Pretty as Core

import           River.Source.Check
import           River.Source.Parser
import qualified River.Source.Pretty as Source
import           River.Source.Syntax
import           River.Source.ToCore

import           River.X64.Color
import qualified River.X64.Pretty as X64
import           River.X64.FromCore

import           System.Environment (getArgs)

import           Text.Trifecta.Delta (Delta)

------------------------------------------------------------------------

main :: IO ()
main = do
  let
    go arg = do
      putStrLn arg
      putStrLn $ take (length arg) (repeat '=')

      p <- runExceptT $ parseProgram arg
      case p of
        Left (TrifectaError xx) ->
          print xx
        Right program -> do
          putStrLn ""
          putStrLn "-- Source --"
          putStrLn ""
          putStrLn $
            Source.displayProgram program

          let
            errors =
              List.sort .
              concatMap ppCheckError .
              checkProgram .
              fmap locationOfDelta $
              program

            core =
              coreOfProgram program

            ecolored =
              coloredOfProgram colorByRegister core

            easm =
              assemblyOfProgram core

          mapM_ (T.putStrLn . ppError) errors

          putStrLn ""
          putStrLn "-- Core --"
          putStrLn ""
          putStrLn $
            Core.displayProgram core

          putStrLn ""
          putStrLn "-- Registers Allocated --"
          putStrLn ""
          putStrLn $
            either show (Core.displayProgram' X64.ppRegister64) ecolored

          putStrLn ""
          putStrLn "-- Assembly (x86-64) --"
          putStrLn ""
          putStrLn $
            either show X64.displayProgram easm

      putStrLn ""

  args <- getArgs
  mapM_ go args

------------------------------------------------------------------------

data Location =
    Location FilePath Int Int
    deriving (Eq, Ord, Read, Show)

locationOfDelta :: Delta -> Location
locationOfDelta d =
  Location (fileOfDelta d) (lineOfDelta d) (columnOfDelta d)

ppError :: (Location, Text) -> Text
ppError (loc, msg) =
  ppLocation loc <> ": error: " <> msg

ppLocation :: Location -> Text
ppLocation (Location file line column) =
  T.pack file <> ":" <>
  T.pack (show line) <> ":" <>
  T.pack (show column)

ppCheckError :: CheckError Location -> [(Location, Text)]
ppCheckError =
  let
    ppUndecl (Identifier n) =
      "undeclared variable '" <> n <> "'"

    ppUninit (Identifier n) =
      "uninitialized variable '" <> n <> "'"

    ppNoRet =
      "return statement not found"
  in \case
    UndeclaredVariable n locs ->
      zip (Set.toList locs) (cycle [ppUndecl n])
    UninitializedVariable n locs ->
      zip (Set.toList locs) (cycle [ppUninit n])
    NoReturnStatement loc ->
      [(loc, ppNoRet)]
