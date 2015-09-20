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

import           River.Source.Check
import           River.Source.Parser
import           River.Source.Pretty
import           River.Source.Syntax

import           System.Environment (getArgs)

import           Text.Trifecta.Delta (Delta)

------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    mapM_ go args
  where
    go arg = do
        p <- runEitherT (parseProgram arg)
        case p of
          Left (TrifectaError xx) -> print xx
          Right program           -> do
            putStrLn (displayProgram program)
            let errs = map snd
                     . List.sort
                     . concatMap ppCheckError
                     $ checkProgram program
            mapM_ T.putStrLn errs

------------------------------------------------------------------------

ppCheckError :: CheckError Delta -> [((FilePath, Int, Int), Text)]
ppCheckError (UndeclaredVariable (Identifier n) ds) =
    map go (Set.toList ds)
  where
    go d = (location d, ppSingle d)

    location d = (fileOfDelta d, lineOfDelta d, columnOfDelta d)

    ppSingle d = T.pack (fileOfDelta d)
              <> ":"
              <> T.pack (show (lineOfDelta d))
              <> ":"
              <> T.pack (show (columnOfDelta d))
              <> ": error: undeclared variable '" <> n <> "'"
