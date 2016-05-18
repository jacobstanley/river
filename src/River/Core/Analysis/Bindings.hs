{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module River.Core.Analysis.Bindings (
    Binding(..)
  , bindingsOfProgram
  , bindingsOfTerm
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           River.Core.Syntax


data Binding n a =
  Binding {
      bindingPre :: ![n]
    , bindingVar :: !n
    , bindingPost :: ![n]
    , bindingTail :: !(Tail n a)
    } deriving (Eq, Ord, Show)

-- | Find the tails associated with each name in the program.
bindingsOfProgram :: Ord n => Program n a -> Map n (Binding n a)
bindingsOfProgram = \case
  Program _ tm ->
    bindingsOfTerm tm

bindingsOfTerm :: Ord n => Term n a -> Map n (Binding n a)
bindingsOfTerm = \case
  Let _ ns tl tm ->
    -- TODO we should probably blow up if we find duplicate names
    mkBindings [] ns tl `Map.union`
    bindingsOfTerm tm

  Return _ _ ->
    Map.empty

mkBindings :: Ord n => [n] -> [n] -> Tail n a -> Map n (Binding n a)
mkBindings pre ns tl =
  case ns of
    [] ->
      Map.empty
    n : post ->
      Map.singleton n (Binding pre n post tl) `Map.union`
      mkBindings (pre ++ [n]) post tl
