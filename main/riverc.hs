module Main (main) where

import Control.Monad.Trans.Either (runEitherT)
import System.Environment (getArgs)

import River.Source.Parser

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
          Right pp                -> print pp
