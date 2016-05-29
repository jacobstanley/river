{-# LANGUAGE LambdaCase #-}
module River.Source.Pretty (
    OutputAnnot(..)
  , displayProgram

  , ppProgram
  , ppStatement
  , ppExpression
  ) where

import qualified Data.Text as T

import           River.Source.Syntax

import           System.Console.ANSI (Color(..), ColorIntensity(..))
import           System.Console.ANSI (ConsoleLayer(..))
import           System.Console.ANSI (SGR(..), setSGRCode)

import           Text.PrettyPrint.Annotated.Leijen (Doc)
import           Text.PrettyPrint.Annotated.Leijen ((<+>), (<>))
import           Text.PrettyPrint.Annotated.Leijen (annotate)
import           Text.PrettyPrint.Annotated.Leijen (text, integer)
import           Text.PrettyPrint.Annotated.Leijen (vcat, indent)
import qualified Text.PrettyPrint.Annotated.Leijen as Pretty

------------------------------------------------------------------------

data OutputAnnot =
    AnnLiteral
  | AnnVariable
  | AnnDeclare
  | AnnAssign
  | AnnKeyword
  | AnnOperator
  | AnnPunctuation
    deriving (Eq, Ord, Read, Show)

------------------------------------------------------------------------

displayProgram :: Program a -> String
displayProgram program =
  let
    doc =
      Pretty.renderPretty 0.8 80 (ppProgram program)

    put attr str =
      sgrAttr attr ++ str ++ sgrReset

    sgrReset =
      setSGRCode [Reset]

    sgrAttr = \case
      AnnLiteral ->
        setSGRCode [SetColor Foreground Dull Red]
      AnnVariable ->
        setSGRCode [SetColor Foreground Dull Green]
      AnnDeclare ->
        setSGRCode [SetColor Foreground Dull Magenta]
      AnnAssign ->
        setSGRCode [SetColor Foreground Dull Cyan]
      AnnKeyword ->
        setSGRCode [SetColor Foreground Dull Blue]
      AnnOperator ->
        setSGRCode [SetColor Foreground Dull Yellow]
      AnnPunctuation ->
        setSGRCode [SetColor Foreground Dull White]
  in
    Pretty.displayDecorated put doc

------------------------------------------------------------------------

ppProgram :: Program a -> Doc OutputAnnot
ppProgram = \case
  Program _ (Block _ ss) ->
    vcat [
        ppKeyword "int" <+>
          annotate AnnDeclare (text "main") <> ppPunctuation "() {"
      , indent 4 (vcat (map ppStatement ss))
      , ppPunctuation "}"
      ]

ppBlock :: Block a -> Doc OutputAnnot
ppBlock = \case
  Block _ [] ->
    vcat [
        ppPunctuation "{"
      , ppPunctuation "}" ]
  Block _ ss ->
    vcat [
        ppPunctuation "{"
      , indent 4 . vcat $ fmap ppStatement ss
      , ppPunctuation "}"
      ]

ppStatement :: Statement a -> Doc OutputAnnot
ppStatement = \case
  Declare _ typ n (Block _ ss) ->
    let
      decl =
        ppType typ <+>
        annotate AnnDeclare (ppIdentifier n) <>
        ppSemi
    in
      vcat $ decl : fmap ppStatement ss

  Assign _ n x ->
    annotate AnnAssign (ppIdentifier n) <+>
    ppOperator "=" <+>
    ppExpression 0 x <>
    ppSemi

  If _ i t (Block _ []) ->
    ppKeyword "if" <+>
    ppParens0 (ppExpression 0 i) <+>
    ppBlock t

  If _ i t (Block _ [e@(If _ _ _ _)]) ->
    ppKeyword "if" <+>
    ppParens0 (ppExpression 0 i) <+>
    ppBlock t <+>
    ppKeyword "else" <+>
    ppStatement e

  If _ i t e ->
    ppKeyword "if" <+>
    ppParens0 (ppExpression 0 i) <+>
    ppBlock t <+>
    ppKeyword "else" <+>
    ppBlock e

  While _ x b ->
    ppKeyword "while" <+>
    ppParens0 (ppExpression 0 x) <+>
    ppBlock b

  Return _ x ->
    ppKeyword "return" <+>
    ppExpression 0 x <>
    ppSemi

ppType :: Type -> Doc OutputAnnot
ppType = \case
  Int ->
    ppKeyword "int"
  Bool ->
    ppKeyword "bool"

ppExpression :: Prec -> Expression a -> Doc OutputAnnot
ppExpression p = \case
  Literal  _ x ->
    annotate AnnLiteral (ppLiteral x)

  Variable _ n ->
    annotate AnnVariable (ppIdentifier n)

  Unary _ op x ->
    let
      pop =
        precUnaryOp op
    in
      ppParens (p > pop) $
        ppUnaryOp op <>
        ppExpression (pop+1) x

  Binary _ op x y ->
    let
      pop =
        precBinaryOp op
    in
      ppParens (p > pop) $
        ppExpression (pop+1) x <+>
        ppBinaryOp op <+>
        ppExpression (pop+1) y

  Conditional _ i t e ->
    let
      pop =
        0
    in
      ppParens (p > pop) $
        ppExpression (pop+1) i <+> ppOperator "?" <+>
        ppExpression (pop+1) t <+> ppOperator ":" <+>
        ppExpression (pop+1) e

ppLiteral :: Literal -> Doc OutputAnnot
ppLiteral = \case
  LiteralInt i ->
    integer i
  LiteralBool True ->
    text "true"
  LiteralBool False ->
    text "false"

------------------------------------------------------------------------

ppIdentifier :: Identifier -> Doc OutputAnnot
ppIdentifier = \case
  Identifier n ->
    text $ T.unpack n

ppUnaryOp :: UnaryOp -> Doc OutputAnnot
ppUnaryOp = \case
  LNot ->
    ppOperator "!"
  BNot ->
    ppOperator "~"
  Neg ->
    ppOperator "-"

ppBinaryOp :: BinaryOp -> Doc OutputAnnot
ppBinaryOp = \case
  Mul ->
    ppOperator "*"
  Div ->
    ppOperator "/"
  Mod ->
    ppOperator "%"
  Add ->
    ppOperator "+"
  Sub ->
    ppOperator "-"
  Shl ->
    ppOperator "<<"
  Shr ->
    ppOperator ">>"
  Lt ->
    ppOperator "<"
  Le ->
    ppOperator "<="
  Gt ->
    ppOperator ">"
  Ge ->
    ppOperator ">="
  Eq ->
    ppOperator "=="
  Ne ->
    ppOperator "!="
  And ->
    ppOperator "&"
  Xor ->
    ppOperator "^"
  Or ->
    ppOperator "|"

ppKeyword :: String -> Doc OutputAnnot
ppKeyword =
  annotate AnnKeyword . text

ppOperator :: String -> Doc OutputAnnot
ppOperator =
  annotate AnnOperator . text

ppPunctuation :: String -> Doc OutputAnnot
ppPunctuation =
  annotate AnnPunctuation . text

ppSemi :: Doc OutputAnnot
ppSemi =
  ppPunctuation ";"

------------------------------------------------------------------------

type Prec = Int

ppParens :: Bool -> Doc OutputAnnot -> Doc OutputAnnot
ppParens ok =
  if ok then
    ppParens0
  else
    id

ppParens0 :: Doc OutputAnnot -> Doc OutputAnnot
ppParens0 doc =
  ppPunctuation "(" <> doc <> ppPunctuation ")"

precUnaryOp :: UnaryOp -> Prec
precUnaryOp = \case
  LNot ->
    11
  BNot ->
    11
  Neg ->
    11

precBinaryOp :: BinaryOp -> Prec
precBinaryOp = \case
  Mul ->
    10
  Div ->
    10
  Mod ->
    10
  Add ->
    9
  Sub ->
    9
  Shl ->
    8
  Shr ->
    8
  Lt ->
    7
  Le ->
    7
  Gt ->
    7
  Ge ->
    7
  Eq ->
    6
  Ne ->
    6
  And ->
    5
  Xor ->
    4
  Or ->
    3
