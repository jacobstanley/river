{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
module River.Source.Analysis.Scope (
    Scope(..)

  , scopedProgram
  , scopedBlock
  , scopedStatement
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import           River.Map
import           River.Source.Annotation
import           River.Source.Syntax


data Scope a =
  Scope {
      scopeDeclared :: !(Map Identifier a)
    , scopeDefined :: !(Map Identifier (Set a))
    , scopeLive :: !(Map Identifier (Set a))
    , scopeTail :: !a
    } deriving (Eq, Ord, Show)

replaceTail :: a -> Scope a -> Scope a
replaceTail a scope =
  scope {
      scopeTail = a
    }

scopedProgram :: Ord a => Program a -> Program (Scope a)
scopedProgram = \case
  Program a0 b0 ->
    let
      b =
        scopedBlock Map.empty b0

      a =
        replaceTail a0 (annotOfBlock b)
    in
      Program a b

scopedBlock :: Ord a => Map Identifier a -> Block a -> Block (Scope a)
scopedBlock declared = \case
  Block a0 [] ->
    let
      a =
        Scope declared Map.empty Map.empty a0
    in
      Block a []

  Block a0 (s0 : t0) ->
    let
      s =
        scopedStatement declared s0

      s_scope =
        annotOfStatement s

      Block t_scope t =
        scopedBlock declared (Block a0 t0)

      -- Defined Variables --

      s_defined =
        scopeDefined s_scope

      t_defined =
        scopeDefined t_scope

      defined =
        s_defined `mapSetUnion` t_defined

      -- Live Variables --

      s_live =
        scopeLive s_scope

      t_live =
        scopeLive t_scope `Map.difference` s_defined

      live =
        s_live `mapSetUnion` t_live

      -- Complete Scope --

      a =
        Scope declared defined live a0
    in
      Block a (s : t)

scopedStatement :: Ord a => Map Identifier a -> Statement a -> Statement (Scope a)
scopedStatement declared = \case
  Declare a0 t n b0 ->
    let
      b =
        scopedBlock (Map.insert n a0 declared) b0

      b_scope =
        annotOfBlock b

      defined =
        Map.delete n $ scopeDefined b_scope

      live =
        Map.delete n $ scopeLive b_scope

      a =
        Scope declared defined live a0
    in
      Declare a t n b

  Assign a0 n x0 ->
    let
      x =
        scopedExpression declared x0

      x_scope =
        annotOfExpression x

      defined =
        mapSetSingleton n a0

      live =
        scopeLive x_scope

      a =
        Scope declared defined live a0
    in
      Assign a n x

  If a0 i0 t0 e0 ->
    let
      i =
        scopedExpression declared i0

      t =
        scopedBlock declared t0

      e =
        scopedBlock declared e0

      i_scope =
        annotOfExpression i

      t_scope =
        annotOfBlock t

      e_scope =
        annotOfBlock e

      defined =
        scopeDefined t_scope `mapSetIntersection`
        scopeDefined e_scope

      live =
        scopeLive i_scope `mapSetUnion`
        scopeLive t_scope `mapSetUnion`
        scopeLive e_scope

      a =
        Scope declared defined live a0
    in
      If a i t e

  While a0 x0 b0 ->
    let
      x =
        scopedExpression declared x0

      b =
        scopedBlock declared b0

      x_scope =
        annotOfExpression x

      b_scope =
        annotOfBlock b

      defined =
        Map.empty

      live =
        scopeLive x_scope `mapSetUnion`
        scopeLive b_scope

      a =
        Scope declared defined live a0
    in
      While a x b

  Return a0 x0 ->
    let
      x =
        scopedExpression declared x0

      x_scope =
        annotOfExpression x

      -- Returning defines all variables in scope, because it transfers control
      -- out of the scope.
      defined =
        Map.map (const $ Set.singleton a0) declared

      live =
        scopeLive x_scope

      a =
        Scope declared defined live a0
    in
      Return a x

scopedExpression :: Ord a => Map Identifier a -> Expression a -> Expression (Scope a)
scopedExpression declared = \case
  Literal a0 l ->
    let
      a =
        Scope declared Map.empty Map.empty a0
    in
      Literal a l

  Variable a0 n ->
    let
      live =
        mapSetSingleton n a0

      a =
        Scope declared Map.empty live a0
    in
      Variable a n

  Unary a0 op x0 ->
    let
      x =
        scopedExpression declared x0

      live =
        scopeLive (annotOfExpression x)

      a =
        Scope declared Map.empty live a0
    in
      Unary a op x

  Binary a0 op x0 y0 ->
    let
      x =
        scopedExpression declared x0

      y =
        scopedExpression declared y0

      live =
        scopeLive (annotOfExpression x) `mapSetUnion`
        scopeLive (annotOfExpression y)

      a =
        Scope declared Map.empty live a0
    in
      Binary a op x y

  Conditional a0 i0 t0 e0 ->
    let
      i =
        scopedExpression declared i0

      t =
        scopedExpression declared t0

      e =
        scopedExpression declared e0

      live =
        scopeLive (annotOfExpression i) `mapSetUnion`
        scopeLive (annotOfExpression t) `mapSetUnion`
        scopeLive (annotOfExpression e)

      a =
        Scope declared Map.empty live a0
    in
      Conditional a i t e
