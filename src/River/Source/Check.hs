module River.Source.Check where

import qualified Data.Map as Map
import           Data.Set (Set)

import           River.Source.Syntax
import           River.Source.Scope

------------------------------------------------------------------------

data CheckError a =
    UndeclaredVariable !Identifier !(Set a)
  deriving (Eq, Ord, Read, Show)

------------------------------------------------------------------------

checkProgram :: Ord a => Program a -> [CheckError a]
checkProgram p = map (\(k,v) -> UndeclaredVariable k v)
                     (Map.toList (fvOfProgram p))
