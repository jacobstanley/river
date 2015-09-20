{-# Language DeriveDataTypeable #-}
{-# Language LambdaCase #-}
{-# Language TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module River.Test.Arbitraries where

import           Data.Char (ord)
import qualified Data.Text as T

import           River.Source.Syntax

import           Test.Feat
import           Test.Feat.Class
import           Test.Feat.Modifiers
import           Test.Feat.Enumerate
import           Test.QuickCheck

------------------------------------------------------------------------

-- Annotation which has a quiet show instance to reduce clutter.
data X = X
  deriving (Eq, Ord, Typeable)

instance Arbitrary X where
  arbitrary = return X
  shrink _  = [X]

instance Show X where
  showsPrec _ X = ("△" ++)

------------------------------------------------------------------------

deriveEnumerable ''X
deriveEnumerable ''UnaryOp
deriveEnumerable ''BinaryOp
deriveEnumerable ''Fragment
deriveEnumerable ''Program

-- Literals cannot be negative
deriveEnumerable'
  . dExcept 'Literal [| unary . funcurry $ \a -> Literal a . nat |]
  $ dAll ''Expression

instance Enumerable Identifier where
  enumerate = Identifier
            . T.pack
            . map identChar
            . nonEmpty
            <$> enumerate

------------------------------------------------------------------------

instance Arbitrary UnaryOp where
  arbitrary = sized uniform

instance Arbitrary BinaryOp where
  arbitrary = sized uniform

instance Arbitrary Identifier where
  arbitrary = sized uniform
  shrink    = \case
    Identifier n
     -> [ Identifier   (T.pack  ns)
        | ns <- shrink (T.unpack n) ]

instance (Enumerable a, Arbitrary a) => Arbitrary (Expression a) where
  arbitrary = sized uniform
  shrink    = \case
    Literal a i
     -> [ Literal as is
        | as <- shrink a
        , is <- shrink i ]

    Variable a i
     -> [ Variable as is
        | as <- shrink a
        , is <- shrink i ]

    Unary a o x
     -> [ Unary as os xs
        | as <- shrink a
        , os <- shrink o
        , xs <- shrink x ]

    Binary a o x y
     -> [ Binary as os xs ys
        | as <- shrink a
        , os <- shrink o
        , xs <- shrink x
        , ys <- shrink y ]

instance (Enumerable a, Arbitrary a) => Arbitrary (Fragment a) where
  arbitrary = sized uniform
  shrink    = \case
    Declaration a i x f
     -> [ Declaration as is xs fs
        | as <- shrink a
        , is <- shrink i
        , xs <- shrink x
        , fs <- shrink f ]

    Assignment a i b x f
     -> [ Assignment as is bs xs fs
        | as <- shrink a
        , is <- shrink i
        , bs <- shrink b
        , xs <- shrink x
        , fs <- shrink f ]

    Return a x
     -> [ Return as xs
        | as <- shrink a
        , xs <- shrink x ]

instance (Enumerable a, Arbitrary a) => Arbitrary (Program a) where
  arbitrary = sized uniform
  shrink    = \case
    Program a s
     -> [ Program as ss
        | as <- shrink a
        , ss <- shrink s ]

------------------------------------------------------------------------

newtype IdentChar = IdentChar { identChar :: Char }
  deriving (Typeable, Show)

instance Enumerable IdentChar where
  enumerate = fmap IdentChar (enumerateBounded (ord 'a') (ord 'z'))

-- I have no idea what this does (stolen from Test.Feat.Modifiers)
enumerateBounded :: Enum a => Int -> Int -> Enumerate a
enumerateBounded from to =
    let e = Enumerate prts (return e) in e
  where
    prts = toRev $ map (\p -> Finite (crd p) (sel p)) ([0..] :: [Integer])
    crd p
       | p <= 0          = 0
       | p == 1          = 1
       | 2^(p-1) > num   = max 0 (num - 2^(p-2))
       | otherwise       = 2^(p-2)
    sel 1 0 = toEnum from
    sel p i = toEnum $ 2^(p-2) + fromInteger i + from
    num    = toInteger $ to - from
