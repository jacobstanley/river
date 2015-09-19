{-# LANGUAGE LambdaCase #-}

module Main (main) where

import           Control.Monad.Trans.Either (runEitherT)

import           River.Source.Parser
import           River.Source.Pretty

import           System.Environment (getArgs)

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
            print program
            putStrLn (displayProgram program)
