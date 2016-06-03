{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module River.Core.Example (
    alpaca_program
  , alpaca_term

  , liveness_program
  , liveness_term
  ) where

import           Control.Monad.Trans.Except (runExceptT)

import           Data.Text (Text)

import           River.Core.Primitive
import           River.Core.Syntax
import           River.Name
import           River.Source.Concrete.Parser
import           River.Source.Elaborate
import           River.Source.ToCore

import           System.IO.Unsafe (unsafePerformIO)

import qualified Text.Megaparsec as Mega


alpaca_program :: Program () Prim (Name Text) ()
alpaca_program =
  fromPath "test/cases/lab1/alpaca.l1"

alpaca_term :: Term () Prim (Name Text) ()
alpaca_term =
  unProgram alpaca_program

liveness_program :: Program () Prim (Name Text) ()
liveness_program =
  fromPath "test/cases/lab1/liveness.l1"

liveness_term :: Term () Prim (Name Text) ()
liveness_term =
  unProgram liveness_program

fromPath :: FilePath -> Program () Prim (Name Text) ()
fromPath path =
  unsafePerformIO $ do
    e <- runExceptT $ parseProgram path
    case e of
      Left xx ->
        error $ Mega.parseErrorPretty xx
      Right p ->
        return . (() <$) . coreOfProgram $ elaborateProgram p

unProgram :: Program () Prim (Name Text) () -> Term () Prim (Name Text) ()
unProgram = \case
  Program _ tm ->
    tm
