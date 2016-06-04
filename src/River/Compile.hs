{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module River.Compile (
    compileBinary
  , CompileError(..)
  , renderCompileError

  , executeBinary
  , executeBinary'
  , ExecuteResult(..)

  -- * should be somewhere else
  , ppError
  , ppParseError
  , ppCheckError
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE)

import           Data.Int (Int64)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Monoid ((<>))
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T

import           River.Bifunctor
import           River.Name
import           River.Source.Check
import           River.Source.Concrete.Parser
import           River.Source.Elaborate
import           River.Source.Syntax
import           River.Source.ToCore
import           River.X64.FromCore
import           River.X64.Pretty
import           River.X64.Syntax

import           System.Directory (renameFile)
import           System.Exit (ExitCode)
import           System.FilePath ((</>))
import           System.IO.Temp (withSystemTempDirectory)
import           System.Process (callProcess, readProcessWithExitCode)

import           Text.Megaparsec (SourcePos(..), Pos, Dec)
import qualified Text.Megaparsec as Mega


data ExecuteResult =
    ExecuteResult !Int64
  | ExecuteError !ExitCode !Text !Text
    deriving (Eq, Ord, Show)

data CompileError =
    ParseError !(Mega.ParseError Char Dec)
  | CheckError ![CheckError SourcePos]
  | X64Error !(X64Error (Name Text) (Maybe SourcePos))
    deriving (Show)

renderCompileError :: [Text] -> CompileError -> Text
renderCompileError file = \case
  ParseError err ->
    ppParseError file err
  CheckError errs ->
    T.unlines .
    fmap (ppError file) .
    List.sort $
    concatMap ppCheckError errs
  X64Error err ->
    T.pack $ show err

------------------------------------------------------------------------

executeBinary :: FilePath -> ExceptT CompileError IO ExecuteResult
executeBinary src =
  executeBinaryE (Right src)

executeBinary' :: String -> ExceptT CompileError IO ExecuteResult
executeBinary' src =
  executeBinaryE (Left src)

executeBinaryE :: Either String FilePath -> ExceptT CompileError IO ExecuteResult
executeBinaryE src = do
  ExceptT . liftIO . withSystemTempDirectory "river" $ \tmp ->
    runExceptT $ do
      let
        executable =
          tmp </> "a.out"

      compileBinaryE src executable

      (code, out, err) <-
        liftIO $ readProcessWithExitCode executable [] ""

      let
        tout =
          T.pack out
        terr =
          T.pack err

      case T.signed T.decimal tout of
        Right (i, "\n") ->
          pure $ ExecuteResult i
        _ ->
          pure $ ExecuteError code tout terr

compileBinary :: FilePath -> FilePath -> ExceptT CompileError IO ()
compileBinary src dst =
  compileBinaryE (Right src) dst

compileBinaryE :: Either String FilePath -> FilePath -> ExceptT CompileError IO ()
compileBinaryE esrc dst = do
  concrete <- firstT ParseError $ either (liftE . parseProgram' "program.c") parseProgram esrc

  let
    program =
      elaborateProgram concrete

  checkProgram' program

  asm <-
    firstT X64Error . liftE .
    assemblyOfProgram Label $
    coreOfProgram program

  ExceptT . liftIO . withSystemTempDirectory "river" $ \tmp ->
    runExceptT $ do
      let
        runtimePath =
          tmp </> "runtime.c"
        programPath =
          tmp </> "program.s"
        outputPath =
          tmp </> "output"

      liftIO $ writeFile runtimePath runtime
      liftIO $ writeFile programPath (displayProgram NoColor asm)
      liftIO $ callProcess "gcc" [runtimePath, programPath, "-o", outputPath]
      liftIO $ renameFile outputPath dst

checkProgram' :: Monad m => Program SourcePos -> ExceptT CompileError m ()
checkProgram' program =
  let
    errors =
      checkProgram $
      program
  in
    case errors of
      [] ->
        return ()
      _ ->
        throwE $ CheckError errors

runtime :: String
runtime =
  unlines
    [ "#include <stdio.h>"
    , "#include <stdlib.h>"
    , ""
    , "extern long long _c0_main();"
    , ""
    , "/* The main function, which calls _c0_main */"
    , "int main() {"
    , "  printf(\"%lld\\n\", _c0_main());"
    , "  exit(0);"
    , "}"
    ]

------------------------------------------------------------------------

ppError :: [Text] -> (SourcePos, Text) -> Text
ppError file (pos, msg) =
  T.pack (Mega.sourcePosPretty pos) <> ": error: " <> msg <> "\n" <>
  ppErrorLine file pos

ppParseError :: [Text] -> Mega.ParseError Char Dec -> Text
ppParseError file err =
  T.pack (Mega.parseErrorPretty err) <> "\n" <>
  ppErrorLine file (NonEmpty.head $ Mega.errorPos err)

ppErrorLine :: [Text] -> SourcePos -> Text
ppErrorLine file pos =
  case drop (fromPos (Mega.sourceLine pos) - 1) file of
    [] ->
      mempty
    errorLine : _ ->
      errorLine <> "\n" <>
      T.replicate (fromPos (Mega.sourceColumn pos) - 1) " " <> "^"

fromPos :: Pos -> Int
fromPos =
  fromIntegral . Mega.unPos

ppCheckError :: CheckError SourcePos -> [(SourcePos, Text)]
ppCheckError =
  let
    ppUndecl (Identifier n) =
      "variable '" <> n <> "' was not declared before use"

    ppUninit (Identifier n) =
      "variable '" <> n <> "' was not initialized"

    ppNoRet =
      "return statement not found"
  in \case
    UndeclaredVariable n loc ->
      [(loc, ppUndecl n)]
    UndefinedVariable n _decl uses->
      zip (Set.toList uses) (cycle [ppUninit n])
    NoReturnStatement loc ->
      [(loc, ppNoRet)]

liftE :: Monad m => Either x a -> ExceptT x m a
liftE =
  ExceptT . pure
