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
import           Text.PrettyPrint.Annotated.Leijen (annotate, noAnnotate)
import           Text.PrettyPrint.Annotated.Leijen (parens, text, integer)
import           Text.PrettyPrint.Annotated.Leijen (semi, empty)
import           Text.PrettyPrint.Annotated.Leijen (vcat, indent)
import qualified Text.PrettyPrint.Annotated.Leijen as Pretty

------------------------------------------------------------------------

data OutputAnnot =
    AnnLiteral
  | AnnVariable
  | AnnDeclaration
  | AnnAssignment
  | AnnKeyword
  | AnnOperator
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
      AnnDeclaration ->
        setSGRCode [SetColor Foreground Dull Magenta]
      AnnAssignment ->
        setSGRCode [SetColor Foreground Dull Cyan]
      AnnKeyword ->
        setSGRCode [SetColor Foreground Dull Blue]
      AnnOperator ->
        setSGRCode [SetColor Foreground Dull Yellow]
  in
    Pretty.displayDecorated put doc

------------------------------------------------------------------------

ppProgram :: Program a -> Doc OutputAnnot
ppProgram = \case
  Program _ ss ->
    vcat [
        ppKeyword "int" <+>
          annotate AnnDeclaration (text "main") <> text "() {"
      , indent 4 (vcat (map ppStatement ss))
      , text "}"
      ]

ppStatement :: Statement a -> Doc OutputAnnot
ppStatement = \case
  Declaration _ n mx ->
    let
      assignment x =
        empty <+> ppOperator "=" <+> ppExpression 0 x
    in
      ppKeyword "int" <+>
      annotate AnnDeclaration (ppIdentifier n) <>
      maybe empty assignment mx <>
      semi

  Assignment _ n op x ->
    annotate AnnAssignment (ppIdentifier n) <+>
    ppAssignOp op <+>
    ppExpression 0 x <>
    semi

  Return _ x ->
    ppKeyword "return" <+>
    ppExpression 0 x <>
    semi

ppExpression :: Prec -> Expression a -> Doc OutputAnnot
ppExpression p = \case
  Literal  _ i ->
    annotate AnnLiteral (integer i)

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

------------------------------------------------------------------------

ppIdentifier :: Identifier -> Doc OutputAnnot
ppIdentifier = \case
  Identifier n ->
    text $ T.unpack n

ppUnaryOp :: UnaryOp -> Doc OutputAnnot
ppUnaryOp = \case
  Neg ->
    ppOperator "-"

ppAssignOp :: Maybe BinaryOp -> Doc OutputAnnot
ppAssignOp = \case
  Nothing ->
    ppOperator "="
  Just op ->
    annotate AnnOperator . noAnnotate $
      ppBinaryOp op <> text "="

ppBinaryOp :: BinaryOp -> Doc OutputAnnot
ppBinaryOp = \case
  Add ->
    ppOperator "+"
  Sub ->
    ppOperator "-"
  Mul ->
    ppOperator "*"
  Div ->
    ppOperator "/"
  Mod ->
    ppOperator "%"

ppKeyword :: String -> Doc OutputAnnot
ppKeyword =
  annotate AnnKeyword . text

ppOperator :: String -> Doc OutputAnnot
ppOperator =
  annotate AnnOperator . text

------------------------------------------------------------------------

type Prec = Int

ppParens :: Bool -> Doc a -> Doc a
ppParens ok =
  if ok then
    parens
  else
    id

precUnaryOp :: UnaryOp -> Prec
precUnaryOp = \case
  Neg ->
    3

precBinaryOp :: BinaryOp -> Prec
precBinaryOp = \case
  Add ->
    1
  Sub ->
    1
  Mul ->
    2
  Div ->
    2
  Mod ->
    2
