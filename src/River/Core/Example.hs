{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module River.Core.Example (
    alpaca_program
  , alpaca_term

  , liveness_program
  , liveness_term
  ) where

import           Data.Text (Text)

import           River.Core.Scope
import           River.Core.Syntax
import           River.Name
import           River.Source.Parser
import           River.Source.ToCore

import           Control.Monad.Trans.Except (runExceptT)
import           System.IO.Unsafe (unsafePerformIO)


alpaca_program :: Program (Name Text) ()
alpaca_program =
  fromPath "test/cases/lab1/alpaca.l1"

alpaca_term :: Term (Name Text) ()
alpaca_term =
  unProgram alpaca_program

liveness_program :: Program (Name Text) ()
liveness_program =
  fromPath "test/cases/lab1/liveness.l1"

liveness_term :: Term (Name Text) ()
liveness_term =
  unProgram liveness_program

fromPath :: FilePath -> Program (Name Text) ()
fromPath path =
  unsafePerformIO $ do
    e <- runExceptT $ parseProgram path
    case e of
      Left (TrifectaError xx) ->
        error $ show xx
      Right p ->
        return . fmap (const ()) $ coreOfProgram p

unProgram :: Program (Name Text) () -> Term (Name Text) ()
unProgram = \case
  Program _ tm ->
    tm
