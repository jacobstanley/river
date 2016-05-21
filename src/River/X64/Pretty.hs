{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module River.X64.Pretty (
    OutputAnnot(..)
  , Decoration(..)
  , displayProgram

  , ppProgram
  , ppInstruction
  , ppRegister64
  ) where

import           Data.Char (toLower)
import           Data.Word (Word64)

import           River.X64.Syntax

import           System.Console.ANSI (Color(..), ColorIntensity(..))
import           System.Console.ANSI (ConsoleLayer(..))
import           System.Console.ANSI (SGR(..), setSGRCode)

import           Text.PrettyPrint.Annotated.Leijen (Doc)
import           Text.PrettyPrint.Annotated.Leijen ((<+>), (<$$>), (<>))
import           Text.PrettyPrint.Annotated.Leijen (annotate, comma, empty)
import           Text.PrettyPrint.Annotated.Leijen (text, integer)
import           Text.PrettyPrint.Annotated.Leijen (vcat, indent)
import qualified Text.PrettyPrint.Annotated.Leijen as Pretty

------------------------------------------------------------------------

data OutputAnnot =
    AnnImmediate
  | AnnRegister
  | AnnInstruction
  | AnnLabel
  | AnnKeyword
    deriving (Eq, Ord, Read, Show)

data Decoration =
    Color
  | NoColor
    deriving (Eq, Ord, Read, Show)

------------------------------------------------------------------------

displayProgram :: Decoration -> [Instruction] -> String
displayProgram decoration instructions =
  let
    doc =
      Pretty.renderPretty 0.8 80 $ vcat
        [ annotate AnnKeyword (text ".globl") <+>
          annotate AnnLabel (text "__c0_main")
        , empty
        , annotate AnnLabel (text "__c0_main:")
        , indent 2 $ ppProgram instructions
        ]

    put attr str =
      case decoration of
        Color ->
          sgrAttr attr ++ str ++ sgrReset
        NoColor ->
          str

    sgrReset =
      setSGRCode [Reset]

    sgrAttr = \case
      AnnImmediate ->
        setSGRCode [SetColor Foreground Dull Red]
      AnnRegister ->
        setSGRCode [SetColor Foreground Dull Magenta]
      AnnInstruction ->
        setSGRCode [SetColor Foreground Dull Cyan]
      AnnLabel ->
        setSGRCode [SetColor Foreground Dull Yellow]
      AnnKeyword ->
        setSGRCode [SetColor Foreground Dull Blue]
  in
    Pretty.displayDecorated put doc

------------------------------------------------------------------------

ppProgram :: [Instruction] -> Doc OutputAnnot
ppProgram = \case
  [] ->
    empty
  x : xs ->
    ppInstruction x <$$>
    ppProgram xs

ppInstruction :: Instruction -> Doc OutputAnnot
ppInstruction = \case
  Movq x y ->
    ppInstructionName "movq" <+> ppOperand64 x <> comma <+> ppOperand64 y
  Negq x ->
    ppInstructionName "negq" <+> ppOperand64 x
  Addq x y ->
    ppInstructionName "addq" <+> ppOperand64 x <> comma <+> ppOperand64 y
  Subq x y ->
    ppInstructionName "subq" <+> ppOperand64 x <> comma <+> ppOperand64 y
  Imulq x ->
    ppInstructionName "imulq" <+> ppOperand64 x
  Cqto ->
    ppInstructionName "cqto"
  Idivq x ->
    ppInstructionName "idivq" <+> ppOperand64 x
  Ret ->
    ppInstructionName "ret"

ppInstructionName :: String -> Doc OutputAnnot
ppInstructionName name =
  annotate AnnInstruction $
    text name

ppOperand64 :: Operand64 -> Doc OutputAnnot
ppOperand64 = \case
  Immediate64 w ->
    annotate AnnImmediate $
      ppImmediate64 w
  Register64 r ->
    annotate AnnRegister $
      ppRegister64 r

ppImmediate64 :: Word64 -> Doc a
ppImmediate64 w =
  "$" <> integer (fromIntegral w)

ppRegister64 :: Register64 -> Doc a
ppRegister64 x =
  "%" <> text (fmap toLower $ show x)
