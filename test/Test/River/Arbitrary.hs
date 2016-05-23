{-# Language DeriveDataTypeable #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.River.Arbitrary (
    X(..)
  ) where

import           Data.Char (ord)
import qualified Data.HashSet as HashSet
import qualified Data.Text as T

import           River.Source.Parser
import           River.Source.Syntax

import           Test.Feat
import           Test.Feat.Class
import           Test.Feat.Modifiers
import           Test.Feat.Enumerate
import           Test.QuickCheck

------------------------------------------------------------------------

-- Annotation which has a quiet show instance to reduce clutter.
data X =
    X
    deriving (Eq, Ord, Typeable)

instance Arbitrary X where
  arbitrary =
    return X

  shrink _ =
    []

instance Show X where
  showsPrec _ X =
    ("△" ++)

------------------------------------------------------------------------

deriveEnumerable ''X
deriveEnumerable ''Type
deriveEnumerable ''UnaryOp
deriveEnumerable ''BinaryOp
deriveEnumerable ''Expression
deriveEnumerable ''Statement
deriveEnumerable ''Block

-- Literals cannot be negative
deriveEnumerable' .
  dExcept 'LiteralInt [| unary $ LiteralInt . nat |] $
  dAll ''Literal

-- Identifiers must not be reserved words or empty
instance Enumerable Identifier where
  enumerate =
    let
      mkIdent xs =
        let
          ident =
            fmap identChar $ nonEmpty xs
        in
          if legalIdent ident then
            Identifier (T.pack ident)
          else
            Identifier "xxx"
    in
      fmap mkIdent enumerate

------------------------------------------------------------------------

instance Arbitrary UnaryOp where
  arbitrary =
    sized uniform

  shrink =
    genericShrink

instance Arbitrary BinaryOp where
  arbitrary =
    sized uniform

  shrink =
    genericShrink

instance Arbitrary Type where
  arbitrary =
    sized uniform

  shrink =
    genericShrink

instance Arbitrary Identifier where
  arbitrary =
    sized uniform `suchThat` \(Identifier ns) ->
      legalIdent (T.unpack ns)

  shrink = \case
    Identifier n ->
      fmap (Identifier . T.pack) .
      filter legalIdent $
      shrink (T.unpack n)

instance Arbitrary Literal where
  arbitrary =
    sized uniform

  shrink =
    genericShrink

instance (Enumerable a, Arbitrary a) => Arbitrary (Expression a) where
  arbitrary =
    sized uniform

  shrink =
    genericShrink

instance (Enumerable a, Arbitrary a) => Arbitrary (Statement a) where
  arbitrary =
    sized uniform

  shrink =
    genericShrink

instance (Enumerable a, Arbitrary a) => Arbitrary (Block a) where
  arbitrary =
    fmap fixBlock $
      Block <$> arbitrary <*> arbitrary

  shrink =
    genericShrink

instance (Enumerable a, Arbitrary a) => Arbitrary (Program a) where
  arbitrary =
    fmap fixProgram $
      Program <$> arbitrary <*> arbitrary

  shrink =
    genericShrink

------------------------------------------------------------------------

fixProgram :: Program a -> Program a
fixProgram = \case
  Program a b ->
    Program a (fixBlock b)

fixBlock :: Block a -> Block a
fixBlock = \case
  Block a ss ->
    Block a (fixStatements ss)

fixStatements :: [Statement a] -> [Statement a]
fixStatements = \case
  [] ->
    []
  Declare a t n (Block ab ss1) : ss2 ->
    [Declare a t n . Block ab . fixStatements $ ss1 ++ ss2]
  s : ss ->
    s : fixStatements ss

------------------------------------------------------------------------

legalIdent :: String -> Bool
legalIdent ident =
  not (null ident) &&
  not (HashSet.member ident reservedNames)

newtype IdentChar =
  IdentChar {
      identChar :: Char
    } deriving (Typeable, Show)

instance Enumerable IdentChar where
  enumerate =
    fmap IdentChar $ enumerateBounded (ord 'a') (ord 'z')

-- I have no idea what this does (stolen from Test.Feat.Modifiers)
enumerateBounded :: Enum a => Int -> Int -> Enumerate a
enumerateBounded from to =
  let
    nats =
      [0..] :: [Integer]

    prts =
      toRev $ fmap (\p -> Finite (crd p) (sel p)) nats

    crd p =
      if p <= 0 then
        0
      else if p == 1 then
       1
      else if 2 ^ (p - 1) > num then
        max 0 (num - 2 ^ (p-2))
      else
        2 ^ (p - 2)

    sel 1 0 =
      toEnum from
    sel p i =
      toEnum $ 2^(p-2) + fromInteger i + from

    num =
      toInteger $ to - from

    enum =
      Enumerate prts (return enum)
  in
    enum
