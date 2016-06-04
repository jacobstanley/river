{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (unless)
import           Control.Monad.Trans.Except (runExceptT)

import           Data.Bifunctor (first)
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           River.Compile
import           River.Core.Analysis.Interference
import           River.Core.Color
import           River.Core.Evaluate
import           River.Core.Fresh
import qualified River.Core.Pretty as Core
import           River.Core.Transform.Coalesce
import           River.Core.Transform.Grail
import           River.Core.Transform.Jump
import           River.Core.Transform.Split
import           River.Fresh
import           River.Source.Check
import           River.Source.Concrete.Parser
import           River.Source.Elaborate
import qualified River.Source.Pretty as Source
import           River.Source.ToCore
import           River.X64.Color
import           River.X64.FromCore
import qualified River.X64.Pretty as X64
import           River.X64.Syntax
import           River.X64.Transform.Recondition
import           River.X64.Transform.Reprim

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
    Left xx -> do
      file <- T.lines <$> T.readFile src
      T.putStrLn . T.strip $ renderCompileError file xx
    Right () ->
      return ()

eval :: FilePath -> IO ()
eval path = do
  p <- runExceptT $ parseProgram path
  case p of
    Left xx ->
      T.putStrLn . flip ppParseError xx . T.lines =<< T.readFile path
    Right concrete ->
      either print print .
      evaluateProgram $
      coreOfProgram $
      elaborateProgram concrete

dump :: FilePath -> IO ()
dump path = do
  putStrLn path
  putStrLn $ take (length path) (repeat '=')

  p <- runExceptT $ parseProgram path
  case p of
    Left xx ->
      T.putStrLn . flip ppParseError xx . T.lines =<< T.readFile path
    Right concrete -> do

      let
        abstract =
          elaborateProgram concrete

      putStrLn ""
      putStrLn "-- Abstract Syntax --"
      putStrLn ""
      putStrLn $
        Source.displayProgram abstract

      let
        errors =
          List.sort .
          concatMap ppCheckError .
          checkProgram $
          abstract

        core =
          coreOfProgram abstract

        recond =
          reconditionProgram core

        e_jump_reprim =
          first show .
          runFreshFrom (nextOfProgram recond) .
          runExceptT $ do
            pp <- reprimProgram recond
            pj <- jumpOfProgram pp
            pure (pj, pp)

        ejump =
          fmap fst e_jump_reprim

        ereprim =
          fmap snd e_jump_reprim

        egrail = do
          pe <- ejump
          pg <- first show $ grailOfProgram pe
          first show $ splitOfProgram pg

        eprecolored =
          first show $ fmap (precoloredOfProgram colorByRegister) egrail

        ecolored =
          first show . coloredOfProgram colorByRegister =<< egrail

        easm =
          first show $ assemblyOfProgram Label core

        fromE e f =
          either id f e

        fromColored (n, mr) =
          maybe (Left n) Right mr

      unless (null errors) $ do
        putStrLn ""
        file <- T.lines <$> T.readFile path
        mapM_ (T.putStrLn . ppError file) errors

      putStrLn ""
      putStrLn "-- Core --"
      putStrLn ""
      putStrLn $
        Core.displayProgram core

      putStrLn ""
      putStrLn "-- Core (with condition codes) --"
      putStrLn ""
      putStrLn $
        Core.displayProgram' X64.ppCc Core.ppPrim Core.ppName recond

      putStrLn ""
      putStrLn "-- Core (with x86-64 primitives) --"
      putStrLn ""
      putStrLn . fromE ereprim $
        Core.displayProgram' X64.ppCc X64.ppPrim Core.ppName

      putStrLn ""
      putStrLn "-- Core (after jump hoisting) --"
      putStrLn ""
      putStrLn . fromE ejump $
        Core.displayProgram' X64.ppCc X64.ppPrim Core.ppName

      putStrLn ""
      putStrLn "-- Core (in grail normal form) --"
      putStrLn ""
      putStrLn . fromE egrail $
        Core.displayProgram' X64.ppCc X64.ppPrim Core.ppName

      putStrLn ""
      putStrLn "-- Core (after precoloring) --"
      putStrLn ""
      putStrLn . fromE eprecolored $
        Core.displayProgram' X64.ppCc X64.ppPrim
          (Core.ppColor Core.ppName X64.ppRegister64)

      putStrLn ""
      putStrLn "-- Interference Graph --"
      putStrLn ""
      putStrLn . fromE eprecolored $
        ppInterferenceGraph (show . Core.ppName) .
        interferenceOfProgram .
        first fst

      putStrLn ""
      putStrLn "-- Registers Allocated --"
      putStrLn ""
      putStrLn . fromE ecolored $
        Core.displayProgram' X64.ppCc X64.ppPrim
          (either Core.ppName X64.ppRegister64) .
        first fromColored

      putStrLn ""
      putStrLn "-- Registers Coalesced --"
      putStrLn ""
      putStrLn . fromE ecolored $
        Core.displayProgram' X64.ppCc X64.ppPrim
          (either Core.ppName X64.ppRegister64) .
        coalesceProgram .
        first fromColored

      putStrLn ""
      putStrLn "-- Assembly (x86-64) --"
      putStrLn ""
      putStrLn . fromE easm $
        X64.displayProgram X64.Color

      putStrLn ""
      putStrLn "-- Core Eval --"
      putStrLn ""
      either print print $
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
