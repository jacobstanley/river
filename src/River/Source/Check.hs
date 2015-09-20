module River.Source.Check where

import qualified Data.Map as Map
import           Data.Set (Set)

import           River.Source.Syntax
import           River.Source.Scope

------------------------------------------------------------------------

data CheckError a =
    UndeclaredVariable    !Identifier !(Set a)
  | UninitializedVariable !Identifier !(Set a)
  | NoReturnStatement  !a
  deriving (Eq, Ord, Read, Show)

------------------------------------------------------------------------

checkProgram :: Ord a => Program a -> [CheckError a]
checkProgram p = checkReturn p
              ++ checkDeclarations p
              ++ checkInitialization p

------------------------------------------------------------------------

checkReturn :: Program a -> [CheckError a]
checkReturn (Program a ss)
    | any isReturn ss = []
    | otherwise       = [NoReturnStatement a]
  where
    isReturn (Return _ _) = True
    isReturn _            = False

checkDeclarations :: Ord a => Program a -> [CheckError a]
checkDeclarations p =
    map go (Map.toList (fvOfProgram p))
  where
    go (k, v) = UndeclaredVariable k v

checkInitialization :: Ord a => Program a -> [CheckError a]
checkInitialization p =
    map go (Map.toList (uvOfProgram p))
  where
    go (k, v) = UninitializedVariable k v
