{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module River.Core.Pretty (
    OutputAnnot(..)
  , displayProgram

  , ppProgram
  , ppTerm
  , ppTail
  , ppAtom
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

displayProgram :: Program a (Name Text) -> String
displayProgram program =
  let
    doc =
      Pretty.renderPretty 0.8 80 (ppProgram program)

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

ppProgram :: Program a (Name Text) -> Doc OutputAnnot
ppProgram = \case
  Program _ tm ->
    ppKeyword "letrec" <+> annotate AnnDefinition "main" <+> ppEquals <$$>
    indent 2 (ppTerm tm)

ppTerm :: Term a (Name Text) -> Doc OutputAnnot
ppTerm = \case
  Let _ ns tl tm ->
    ppCommaSep (map (annotate AnnDefinition . ppName) ns) <+> ppEquals <+> ppTail tl <$$>
    ppTerm tm
  Return _ tl ->
    ppKeyword "return" <+> ppTail tl

ppTail :: Tail a (Name Text) -> Doc OutputAnnot
ppTail = \case
  Copy _ [] ->
    ppUnit
  Copy _ as ->
    ppCommaSep (map ppAtom as)
  Unary _ op x ->
    ppUnaryOp  op <+> ppAtom x
  Binary _ op x y ->
    ppBinaryOp op <+> ppCommaSep [ppAtom x, ppAtom y]

ppAtom :: Atom a (Name Text) -> Doc OutputAnnot
ppAtom = \case
  Immediate _ i ->
    annotate AnnImmediate (integer i)
  Variable  _ n ->
    annotate AnnUsage (ppName n)

ppUnaryOp :: UnaryOp -> Doc OutputAnnot
ppUnaryOp = \case
  Neg ->
    ppPrimitive "neg"

ppBinaryOp :: BinaryOp -> Doc OutputAnnot
ppBinaryOp = \case
  Add ->
    ppPrimitive "add"
  Sub ->
    ppPrimitive "sub"
  Mul ->
    ppPrimitive "mul"
  Div ->
    ppPrimitive "div"
  Mod ->
    ppPrimitive "mod"

------------------------------------------------------------------------

ppName :: Name Text -> Doc OutputAnnot
ppName = \case
  Name n ->
    text (T.unpack n)
  NameMod n i ->
    text (T.unpack n) <> "%" <> int i
  NameNew i ->
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
