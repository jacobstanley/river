module River.Source.Check where

import qualified Data.Map as Map
import           Data.Set (Set)

import           River.Source.Syntax
import           River.Source.Scope

------------------------------------------------------------------------

data CheckError a =
    UndeclaredVariable !Identifier !(Set a)
  | NoReturnStatement  !a
  deriving (Eq, Ord, Read, Show)

------------------------------------------------------------------------

checkProgram :: Ord a => Program a -> [CheckError a]
checkProgram p = checkReturn p
              ++ checkVariables p

------------------------------------------------------------------------

checkReturn :: Program a -> [CheckError a]
checkReturn (Program a ss)
    | any isReturn ss = []
    | otherwise       = [NoReturnStatement a]
  where
    isReturn (Return _ _) = True
    isReturn _            = False

checkVariables :: Ord a => Program a -> [CheckError a]
checkVariables p =
    map undecl (Map.toList (fvOfProgram p))
  where
    undecl (k, v) = UndeclaredVariable k v
