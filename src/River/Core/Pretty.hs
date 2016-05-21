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

import           River.Core.Primitive
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

displayProgram :: Program Prim (Name Text) a -> String
displayProgram =
  displayProgram' ppPrim ppName

displayProgram' ::
  (p -> Doc OutputAnnot) ->
  (n -> Doc OutputAnnot) ->
  Program p n a ->
  String
displayProgram' ppP ppN program =
  let
    doc =
      Pretty.renderPretty 0.8 80 (ppProgram ppP ppN program)

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

ppProgram :: (p -> Doc OutputAnnot) -> (n -> Doc OutputAnnot) -> Program p n a -> Doc OutputAnnot
ppProgram ppP ppN = \case
  Program _ tm ->
    ppKeyword "letrec" <+> annotate AnnDefinition "main" <+> ppEquals <$$>
    indent 2 (ppTerm ppP ppN tm)

ppTerm :: (p -> Doc OutputAnnot) -> (n -> Doc OutputAnnot) -> Term p n a -> Doc OutputAnnot
ppTerm ppP ppN = \case
  Let _ ns tl tm ->
    ppCommaSep (map (annotate AnnDefinition . ppN) ns) <+> ppEquals <+> ppTail ppP ppN tl <$$>
    ppTerm ppP ppN tm
  Return _ tl ->
    ppKeyword "return" <+> ppTail ppP ppN tl

ppTail :: (p -> Doc OutputAnnot) -> (n -> Doc OutputAnnot) -> Tail p n a -> Doc OutputAnnot
ppTail ppP ppN = \case
  Copy _ [] ->
    ppUnit
  Copy _ as ->
    ppCommaSep (map (ppAtom ppN) as)
  Prim _ prim [] ->
    annotate AnnPrimitive (ppP prim) <+> ppUnit
  Prim _ prim as ->
    annotate AnnPrimitive (ppP prim) <+> ppCommaSep (map (ppAtom ppN) as)

ppAtom :: (n -> Doc OutputAnnot) -> Atom n a -> Doc OutputAnnot
ppAtom ppN = \case
  Immediate _ i ->
    annotate AnnImmediate (integer i)
  Variable  _ n ->
    annotate AnnUsage (ppN n)

ppPrim :: Prim -> Doc a
ppPrim = \case
  Neg ->
    text "neg"
  Add ->
    text "add"
  Sub ->
    text "sub"
  Mul ->
    text "mul"
  Div ->
    text "div"
  Mod ->
    text "mod"

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

ppKeyword :: String -> Doc OutputAnnot
ppKeyword =
  annotate AnnKeyword . text
