module River.Map (
    mapDifferenceSet
  , mapIntersectionSet
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)


mapDifferenceSet :: Ord k => Map k v -> Set k -> Map k v
mapDifferenceSet m =
  Map.difference m . Map.fromSet (const ())

mapIntersectionSet :: Ord k => Map k v -> Set k -> Map k v
mapIntersectionSet m =
  Map.intersection m . Map.fromSet (const ())
