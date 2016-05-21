{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module River.Compile (
    compileBinary

  , CompileError(..)
  , renderCompileError

  -- * should be somewhere else
  , ppError
  , ppCheckError
  , locationOfDelta
  ) where

import           Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE)
import           Control.Monad.IO.Class (liftIO)

import           Data.Monoid ((<>))
import qualified Data.List as List
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T

import           River.Bifunctor.Trans
import           River.Name
import           River.Source.Check
import           River.Source.Parser
import           River.Source.Syntax
import           River.Source.ToCore
import           River.X64.FromCore
import           River.X64.Pretty

import           System.Directory (renameFile)
import           System.FilePath ((</>))
import           System.IO.Temp (withSystemTempDirectory)
import           System.Process (callProcess)

import           Text.Trifecta.Delta (Delta)


data CompileError =
    ParseError !ParseError
  | CheckError ![CheckError Location]
  | X64Error !(X64Error (Name Text) (Maybe Location))
    deriving (Show)

renderCompileError :: CompileError -> Text
renderCompileError = \case
  ParseError err ->
    T.pack $ show err
  CheckError errs ->
    T.unlines .
    fmap ppError . 
    List.sort $
    concatMap ppCheckError errs
  X64Error err ->
    T.pack $ show err

------------------------------------------------------------------------

compileBinary :: FilePath -> FilePath -> ExceptT CompileError IO ()
compileBinary src dst = do
  program <- firstT ParseError $ parseProgram src
  checkProgram' program

  asm <-
    firstT (X64Error . fmap (fmap locationOfDelta)) . liftE .
    assemblyOfProgram $
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

checkProgram' :: Monad m => Program Delta -> ExceptT CompileError m ()
checkProgram' program =
  let
    errors =
      checkProgram .
      fmap locationOfDelta $
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
    , "extern int _c0_main();"
    , ""
    , "/* The main function, which calls _c0_main */"
    , "int main() {"
    , "  printf(\"%d\\n\", _c0_main());"
    , "  exit(0);"
    , "}"
    ]

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

liftE :: Monad m => Either x a -> ExceptT x m a
liftE =
  ExceptT . pure
