{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module River.Core.Pretty (
    OutputAnnot(..)
  , displayProgram
  , displayProgram'
  , displayDoc

  , ppProgram
  , ppTerm
  , ppTail
  , ppAtom

  , ppPrim
  , ppName
  , ppIntName
  , ppColor
  , ppColor'
  , ppKEmpty
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as List

import           River.Core.Primitive
import           River.Core.Syntax
import           River.Name

import           System.Console.ANSI (Color(..), ColorIntensity(..))
import           System.Console.ANSI (ConsoleLayer(..))
import           System.Console.ANSI (SGR(..), setSGRCode)

import           Text.PrettyPrint.Annotated.Leijen (Doc)
import           Text.PrettyPrint.Annotated.Leijen ((<+>), (<$$>), (<>))
import           Text.PrettyPrint.Annotated.Leijen (annotate, hcat, vcat, indent)
import           Text.PrettyPrint.Annotated.Leijen (space, comma, punctuate, empty)
import           Text.PrettyPrint.Annotated.Leijen (text, int, integer)
import qualified Text.PrettyPrint.Annotated.Leijen as Pretty

------------------------------------------------------------------------

data OutputAnnot =
    AnnImmediate
  | AnnPrimitive
  | AnnUsage
  | AnnDefinition
  | AnnFunction
  | AnnCall
  | AnnOperator
  | AnnKeyword
  | AnnColor
    deriving (Eq, Ord, Read, Show)

------------------------------------------------------------------------

displayProgram :: Program () Prim (Name Text) a -> String
displayProgram =
  displayProgram' ppKEmpty ppPrim ppName

displayProgram' ::
  (k -> Doc OutputAnnot) ->
  (p -> Doc OutputAnnot) ->
  (n -> Doc OutputAnnot) ->
  Program k p n a ->
  String
displayProgram' ppK ppP ppN program =
  displayDoc $
    ppProgram ppK ppP ppN program

displayDoc :: Doc OutputAnnot -> String
displayDoc =
  let
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
      AnnFunction ->
        setSGRCode [SetColor Foreground Vivid Magenta]
      AnnCall ->
        setSGRCode [SetColor Foreground Vivid Green]
      AnnOperator ->
        setSGRCode [SetColor Foreground Dull Yellow]
      AnnKeyword ->
        setSGRCode [SetColor Foreground Dull Blue]
      AnnColor ->
        setSGRCode [SetColor Foreground Dull White]
  in
    Pretty.displayDecorated put .
    Pretty.renderPretty 0.8 80

------------------------------------------------------------------------

ppProgram ::
  (k -> Doc OutputAnnot) ->
  (p -> Doc OutputAnnot) ->
  (n -> Doc OutputAnnot) ->
  Program k p n a ->
  Doc OutputAnnot
ppProgram ppK ppP ppN = \case
  Program _ tm ->
    ppKeyword "letrec" <+> annotate AnnFunction "main" <+> ppEquals <$$>
    indent 2 (ppTerm ppK ppP ppN tm)

ppTerm ::
  (k -> Doc OutputAnnot) ->
  (p -> Doc OutputAnnot) ->
  (n -> Doc OutputAnnot) ->
  Term k p n a ->
  Doc OutputAnnot
ppTerm ppK ppP ppN = \case
  Return _ tl ->
    ppKeyword "return" <+> ppTail ppP ppN tl

  If _ k i t e ->
    vcat [
        annotate AnnKeyword (text "if" <> ppK k) <+> ppAtom ppN i <+> ppKeyword "then"
      , indent 2 $ ppTerm ppK ppP ppN t
      , ppKeyword "else"
      , indent 2 $ ppTerm ppK ppP ppN e
      ]

  Let _ ns tl tm ->
    ppNames ppN ns <+> ppEquals <+> ppTail ppP ppN tl <$$>
    ppTerm ppK ppP ppN tm

  LetRec _ (Bindings _ bs) tm ->
    vcat [
        ppKeyword "letrec"
      , indent 2 . vcat . List.intersperse empty $ fmap (uncurry $ ppBinding ppK ppP ppN) bs
      , ppKeyword "in"
      , indent 2 $ ppTerm ppK ppP ppN tm
      ]

ppBinding ::
  (k -> Doc OutputAnnot) ->
  (p -> Doc OutputAnnot) ->
  (n -> Doc OutputAnnot) ->
  n ->
  Binding k p n a ->
  Doc OutputAnnot
ppBinding ppK ppP ppN n = \case
  Lambda _ ns tm ->
    annotate AnnFunction (ppN n) <+>
    ppNames ppN ns <+>
    ppEquals <$$>
    indent 2 (ppTerm ppK ppP ppN tm)

ppNames :: (n -> Doc OutputAnnot) -> [n] -> Doc OutputAnnot
ppNames ppN = \case
  [] ->
    ppUnit
  ns ->
    ppCommaSep (map (annotate AnnDefinition . ppN) ns)

ppTail :: (p -> Doc OutputAnnot) -> (n -> Doc OutputAnnot) -> Tail p n a -> Doc OutputAnnot
ppTail ppP ppN = \case
  Copy _ [] ->
    ppUnit
  Copy _ as ->
    ppCommaSep (map (ppAtom ppN) as)
  Call _ n [] ->
    annotate AnnCall (ppN n) <+> ppUnit
  Call _ n as ->
    annotate AnnCall (ppN n) <+> ppCommaSep (map (ppAtom ppN) as)
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
  Not ->
    text "not"
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
  Lt ->
    text "lt"
  Le ->
    text "le"
  Gt ->
    text "gt"
  Ge ->
    text "ge"
  Eq ->
    text "eq"
  Ne ->
    text "ne"
  And ->
    text "and"
  Xor ->
    text "xor"
  Or ->
    text "or"
  Shl ->
    text "shl"
  Shr ->
    text "shr"

------------------------------------------------------------------------

ppName :: Name Text -> Doc OutputAnnot
ppName = \case
  Name n ->
    text (T.unpack n)
  NameMod n i ->
    text (T.unpack n) <> "%" <> int i
  NameNew i ->
    "%" <> int i

ppColor ::
  (n -> Doc OutputAnnot) ->
  (c -> Doc OutputAnnot) ->
  (n, Maybe c) ->
  Doc OutputAnnot
ppColor ppN ppC =
 ppColor' " " ppN ppC

ppColor' ::
  Doc OutputAnnot ->
  (n -> Doc OutputAnnot) ->
  (c -> Doc OutputAnnot) ->
  (n, Maybe c) ->
  Doc OutputAnnot
ppColor' sep ppN ppC = \case
  (n, Nothing) ->
    ppN n
  (n, Just c) ->
    ppN n <> annotate AnnOperator sep <> annotate AnnColor (ppC c)

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

ppKEmpty :: () -> Doc OutputAnnot
ppKEmpty _ =
  empty

ppKeyword :: String -> Doc OutputAnnot
ppKeyword =
  annotate AnnKeyword . text
