{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module River.Core.Pretty (
    OutputAnnot(..)
  , displayProgram
  , displayProgram'

  , ppProgram
  , ppTerm
  , ppTail
  , ppAtom

  , ppName
  , ppIntName
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

import           River.Core.Syntax
import           River.Name

import           System.Console.ANSI (Color(..), ColorIntensity(..))
import           System.Console.ANSI (ConsoleLayer(..))
import           System.Console.ANSI (SGR(..), setSGRCode)

import           Text.PrettyPrint.Annotated.Leijen (Doc)
import           Text.PrettyPrint.Annotated.Leijen ((<+>), (<$$>), (<>))
import           Text.PrettyPrint.Annotated.Leijen (annotate, hcat, indent)
import           Text.PrettyPrint.Annotated.Leijen (space, comma, punctuate)
import           Text.PrettyPrint.Annotated.Leijen (text, int, integer)
import qualified Text.PrettyPrint.Annotated.Leijen as Pretty

------------------------------------------------------------------------

data OutputAnnot =
    AnnImmediate
  | AnnPrimitive
  | AnnUsage
  | AnnDefinition
  | AnnOperator
  | AnnKeyword
    deriving (Eq, Ord, Read, Show)

------------------------------------------------------------------------

displayProgram :: Program (Name Text) a -> String
displayProgram =
  displayProgram' ppName

displayProgram' :: (n -> Doc OutputAnnot) -> Program n a -> String
displayProgram' ppN program =
  let
    doc =
      Pretty.renderPretty 0.8 80 (ppProgram ppN program)

    put attr str =
      sgrAttr attr ++ str ++ sgrReset

    sgrReset =
      setSGRCode [Reset]

    sgrAttr = \case
      AnnImmediate ->
        setSGRCode [SetColor Foreground Dull Red]
      AnnPrimitive ->
        setSGRCode [SetColor Foreground Dull Cyan]
      AnnUsage ->
        setSGRCode [SetColor Foreground Dull Green]
      AnnDefinition ->
        setSGRCode [SetColor Foreground Dull Magenta]
      AnnOperator ->
        setSGRCode [SetColor Foreground Dull Yellow]
      AnnKeyword ->
        setSGRCode [SetColor Foreground Dull Blue]
  in
    Pretty.displayDecorated put doc

------------------------------------------------------------------------

ppProgram :: (n -> Doc OutputAnnot) -> Program n a -> Doc OutputAnnot
ppProgram ppN = \case
  Program _ tm ->
    ppKeyword "letrec" <+> annotate AnnDefinition "main" <+> ppEquals <$$>
    indent 2 (ppTerm ppN tm)

ppTerm :: (n -> Doc OutputAnnot) -> Term n a -> Doc OutputAnnot
ppTerm ppN = \case
  Let _ ns tl tm ->
    ppCommaSep (map (annotate AnnDefinition . ppN) ns) <+> ppEquals <+> ppTail ppN tl <$$>
    ppTerm ppN tm
  Return _ tl ->
    ppKeyword "return" <+> ppTail ppN tl

ppTail :: (n -> Doc OutputAnnot) -> Tail n a -> Doc OutputAnnot
ppTail ppN = \case
  Copy _ [] ->
    ppUnit
  Copy _ as ->
    ppCommaSep (map (ppAtom ppN) as)
  Prim _ prim [] ->
    ppPrim prim <+> ppUnit
  Prim _ prim as ->
    ppPrim prim <+> ppCommaSep (map (ppAtom ppN) as)

ppAtom :: (n -> Doc OutputAnnot) -> Atom n a -> Doc OutputAnnot
ppAtom ppN = \case
  Immediate _ i ->
    annotate AnnImmediate (integer i)
  Variable  _ n ->
    annotate AnnUsage (ppN n)

ppPrim :: Prim -> Doc OutputAnnot
ppPrim = \case
  Neg ->
    ppPrimitive "neg"
  Add ->
    ppPrimitive "add"
  Sub ->
    ppPrimitive "sub"
  Mul ->
    ppPrimitive "mul"
  DivMod ->
    ppPrimitive "divmod"

------------------------------------------------------------------------

ppName :: Name Text -> Doc OutputAnnot
ppName = \case
  Name n ->
    text (T.unpack n)
  NameMod n i ->
    text (T.unpack n) <> "%" <> int i
  NameNew i ->
    "%" <> int i

ppIntName :: Int -> Doc OutputAnnot
ppIntName i =
  "%" <> int i

ppCommaSep :: [Doc OutputAnnot] -> Doc OutputAnnot
ppCommaSep =
  hcat . punctuate (annotate AnnOperator comma <> space)

ppEquals :: Doc OutputAnnot
ppEquals =
  annotate AnnOperator (text "=")

ppUnit :: Doc OutputAnnot
ppUnit =
  annotate AnnOperator (text "()")

ppPrimitive :: String -> Doc OutputAnnot
ppPrimitive =
  annotate AnnPrimitive . text

ppKeyword :: String -> Doc OutputAnnot
ppKeyword =
  annotate AnnKeyword . text
