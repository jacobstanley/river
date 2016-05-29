{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module River.X64.Pretty (
    OutputAnnot(..)
  , Decoration(..)
  , displayProgram

  , ppProgram
  , ppInstruction
  , ppRegister64

  , ppPrim
  ) where

import           Data.Char (toLower)
import qualified Data.Text as T
import           Data.Word (Word64)

import           River.Name
import qualified River.X64.Primitive as X64
import           River.X64.Syntax

import           System.Console.ANSI (Color(..), ColorIntensity(..))
import           System.Console.ANSI (ConsoleLayer(..))
import           System.Console.ANSI (SGR(..), setSGRCode)

import           Text.PrettyPrint.Annotated.Leijen (Doc)
import           Text.PrettyPrint.Annotated.Leijen ((<+>), (<>))
import           Text.PrettyPrint.Annotated.Leijen (annotate, comma, colon, empty)
import           Text.PrettyPrint.Annotated.Leijen (text, int, integer)
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
    c0main =
      Label $ Name "__c0_main"

    doc =
      Pretty.renderPretty 0.8 80 $ vcat
        [ annotate AnnKeyword (text ".globl") <+>
          ppLabel c0main
        , empty
        , ppLabel c0main <> text ":"
        , ppProgram instructions
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
ppProgram =
  vcat . fmap ppInstruction

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
  Cmpq x y ->
    ppInstructionName "cmpq" <+> ppOperand64 x <> comma <+> ppOperand64 y
  Test x y ->
    ppInstructionName "test" <+> ppOperand64 x <> comma <+> ppOperand64 y
  Set cc x ->
    ppInstructionName' "set" (ppCc cc) <+> ppOperand64 x
  J cc l ->
    ppInstructionName' "j" (ppCc cc) <+> ppLabel l
  Jmp l ->
    ppInstructionName "jmp" <+> ppLabel l
  Lbl l ->
    ppLabel l <> colon
  Ret ->
    ppInstructionName "ret"

ppInstructionName :: String -> Doc OutputAnnot
ppInstructionName name =
  ppInstructionName' name empty

ppInstructionName' :: String -> Doc OutputAnnot -> Doc OutputAnnot
ppInstructionName' name imod =
  indent 2 . annotate AnnInstruction $
    text name <> imod

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

ppLabel :: Label -> Doc OutputAnnot
ppLabel (Label xx) =
  annotate AnnLabel $
  case xx of
    Name n ->
      text (T.unpack n)
    NameMod n i ->
      text (T.unpack n) <> "_" <> int i
    NameNew i ->
      text "river_" <> int i

ppPrim :: X64.Prim -> Doc a
ppPrim = \case
  X64.Neg ->
    text "neg"
  X64.Not ->
    text "not"
  X64.Add ->
    text "add"
  X64.Sub ->
    text "sub"
  X64.Imul ->
    text "imul"
  X64.Idiv ->
    text "idiv"
  X64.Cqto ->
    text "cqto"
  X64.And ->
    text "and"
  X64.Xor ->
    text "xor"
  X64.Or ->
    text "or"
  X64.Sal ->
    text "sal"
  X64.Sar ->
    text "sar"
  X64.Cmp ->
    text "cmp"
  X64.Set cc ->
    text "set" <> ppCc cc

ppCc :: Cc -> Doc a
ppCc = \case
  Z ->
    text "z"
  E ->
    text "e"
  Nz ->
    text "nz"
  Ne ->
    text "ne"
  L ->
    text "l"
  Le ->
    text "le"
  Gt ->
    text "g"
  G ->
    text "ge"
