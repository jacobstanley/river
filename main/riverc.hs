{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Trans.Except (runExceptT)

import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           River.Compile
import           River.Core.Color
import qualified River.Core.Pretty as Core
import           River.Source.Check
import           River.Source.Parser
import qualified River.Source.Pretty as Source
import           River.Source.ToCore
import           River.X64.Color
import qualified River.X64.Pretty as X64
import           River.X64.FromCore

import           System.Environment (getArgs)

------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("dump" : paths) ->
      mapM_ dump paths
    ("compile" : src : dst : []) ->
      compile src dst
    _ -> do
      putStrLn "Usage:"
      putStrLn ""
      putStrLn "  riverc COMMAND"
      putStrLn ""
      putStrLn "Commands:"
      putStrLn ""
      putStrLn "  dump PATH [PATH] [PATH]"
      putStrLn ""
      putStrLn "  compile SOURCE_PATH OUTPUT_PATH"
      putStrLn ""

------------------------------------------------------------------------

compile :: FilePath -> FilePath -> IO ()
compile src dst = do
  e <- runExceptT $ compileBinary src dst
  case e of
    Left xx ->
      T.putStrLn . T.strip $ renderCompileError xx
    Right () ->
      return ()

dump :: FilePath -> IO ()
dump path = do
  putStrLn path
  putStrLn $ take (length path) (repeat '=')

  p <- runExceptT $ parseProgram path
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
        either show (X64.displayProgram X64.Color) easm
