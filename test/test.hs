import           Control.Monad (unless)

import           System.Exit (exitFailure)
import           System.IO (BufferMode(..), hSetBuffering, stdout, stderr)

import qualified Test.River.Evaluation
import qualified Test.River.Parser


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  results <- sequence tests
  unless (and results) exitFailure

tests :: [IO Bool]
tests = [
    Test.River.Parser.tests
  -- , Test.River.Evaluation.tests
  ]
