{-# LANGUAGE TupleSections #-}
module River.Map (
    mapDifferenceSet
  , mapDifferenceList
  , mapIntersectionSet

  , mapSetSingleton
  , mapSetSingleton'
  , mapSetUnion
  , mapSetUnions
  , mapSetIntersection
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set


mapDifferenceSet :: Ord k => Map k v -> Set k -> Map k v
mapDifferenceSet m =
  Map.difference m . Map.fromSet (const ())

mapDifferenceList :: Ord k => Map k v -> [k] -> Map k v
mapDifferenceList m =
  Map.difference m . Map.fromList . fmap (,())

mapIntersectionSet :: Ord k => Map k v -> Set k -> Map k v
mapIntersectionSet m =
  Map.intersection m . Map.fromSet (const ())

mapSetSingleton :: k -> a -> Map k (Set a)
mapSetSingleton n a =
  Map.singleton n (Set.singleton a)

mapSetSingleton' :: k -> Map k (Set a)
mapSetSingleton' n =
  Map.singleton n Set.empty

mapSetUnion :: (Ord k, Ord a) => Map k (Set a) -> Map k (Set a) -> Map k (Set a)
mapSetUnion xs ys =
  Map.unionWith Set.union xs ys

mapSetUnions :: (Ord k, Ord a) => [Map k (Set a)] -> Map k (Set a)
mapSetUnions xss =
  Map.unionsWith Set.union xss

mapSetIntersection :: (Ord k, Ord a) => Map k (Set a) -> Map k (Set a) -> Map k (Set a)
mapSetIntersection xs ys =
  Map.intersectionWith Set.union xs ys
