{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (unless)
import           Control.Monad.Trans.Except (runExceptT)

import           Data.Bifunctor (first)
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           River.Compile
import           River.Core.Color
import           River.Core.Evaluator
import           River.Core.Fresh
import qualified River.Core.Pretty as Core
import           River.Fresh
import           River.Source.Check
import           River.Source.Parser
import qualified River.Source.Pretty as Source
import           River.Source.ToCore
import           River.X64.Assimilate
import           River.X64.Color
import           River.X64.FromCore
import qualified River.X64.Pretty as X64

import           System.Environment (getArgs)

------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("dump" : paths) ->
      mapM_ dump paths
    ("eval" : src : []) ->
      eval src
    ("compile" : src : []) ->
      compile src "a.out"
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
      putStrLn "  eval PATH"
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

eval :: FilePath -> IO ()
eval path = do
  p <- runExceptT $ parseProgram path
  case p of
    Left (TrifectaError xx) ->
      print xx
    Right source ->
      either (print . fmap (fmap locationOfDelta)) print .
      evaluateProgram $
      coreOfProgram source

dump :: FilePath -> IO ()
dump path = do
  putStrLn path
  putStrLn $ take (length path) (repeat '=')

  p <- runExceptT $ parseProgram path
  case p of
    Left (TrifectaError xx) ->
      print xx
    Right source -> do
      putStrLn ""
      putStrLn "-- Source --"
      putStrLn ""
      putStrLn $
        Source.displayProgram source

      let
        errors =
          List.sort .
          concatMap ppCheckError .
          checkProgram .
          fmap locationOfDelta $
          source

        core =
          coreOfProgram source

        eassim =
          first show .
          runFreshFrom (nextOfProgram core) .
          runExceptT $
            assimilateProgram core

        ecolored =
          first show . coloredOfProgram colorByRegister =<< eassim

        easm =
          first show $ assemblyOfProgram core

      unless (null errors) $ do
        putStrLn ""
        mapM_ (T.putStrLn . ppError) errors

      putStrLn ""
      putStrLn "-- Core --"
      putStrLn ""
      putStrLn $
        Core.displayProgram core

      putStrLn ""
      putStrLn "-- Core (with x86-64 primitives) --"
      putStrLn ""
      putStrLn $
        either id (Core.displayProgram' X64.ppPrim Core.ppName) eassim

      putStrLn ""
      putStrLn "-- Registers Allocated --"
      putStrLn ""
      putStrLn $
        either id (Core.displayProgram' X64.ppPrim X64.ppRegister64) ecolored

      putStrLn ""
      putStrLn "-- Assembly (x86-64) --"
      putStrLn ""
      putStrLn $
        either id (X64.displayProgram X64.Color) easm

      putStrLn ""
      putStrLn "-- Core Eval --"
      putStrLn ""
      either (print . fmap (fmap locationOfDelta)) print $
        evaluateProgram core

      putStrLn ""
      putStrLn "-- x86-64 Eval --"
      putStrLn ""
      eresult <- runExceptT $ executeBinary path
      case eresult of
        Left _ ->
          putStrLn "(compilation failed)"
        Right (ExecuteResult res) ->
          print res
        Right err ->
          print err

      putStrLn ""
