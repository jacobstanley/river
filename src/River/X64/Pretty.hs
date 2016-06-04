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
  , ppCc
  ) where

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
    ppInstruction' "movq" [x, y]
  Negq x ->
    ppInstruction' "negq" [x]
  Addq x y ->
    ppInstruction' "addq" [x, y]
  Subq x y ->
    ppInstruction' "subq" [x, y]
  Imulq x ->
    ppInstruction' "imulq" [x]
  Cqto ->
    ppInstruction' "cqto" []
  Idivq x ->
    ppInstruction' "idivq" [x]
  Movzbq (Register64 x) y ->
    ppInstructionName "movzbq"
      <+> annotate AnnRegister (ppRegister8 x) <> comma
      <+> ppOperand y
  Movzbq x y ->
    ppInstruction' "movzbq" [x, y] -- not legit
  Lahf ->
    ppInstruction' "lahf" []
  Sahf ->
    ppInstruction' "sahf" []
  Cmpq x y ->
    ppInstruction' "cmpq" [x, y]
  Test x y ->
    ppInstruction' "test" [x, y]
  Set cc (Register64 x) ->
    ppInstructionName' "set" (ppCc cc)
      <+> annotate AnnRegister (ppRegister8 x)
  Set cc x ->
    ppInstructionName' "set" (ppCc cc)
      <+> ppOperand x
  J cc l ->
    ppInstructionName' "j" (ppCc cc)
      <+> ppLabel l
  Jmp l ->
    ppInstructionName "jmp"
      <+> ppLabel l
  Lbl l ->
    ppLabel l <> colon
  Ret ->
    ppInstruction' "ret" []

ppInstruction' :: String -> [Operand64] -> Doc OutputAnnot
ppInstruction' name args =
  ppInstructionName name <> ppOperands args

ppOperands :: [Operand64] -> Doc OutputAnnot
ppOperands = \case
  [] ->
    empty
  [x] ->
    empty <+> ppOperand x
  x : xs ->
    empty <+> ppOperand x <> comma <> ppOperands xs

ppInstructionName :: String -> Doc OutputAnnot
ppInstructionName name =
  ppInstructionName' name empty

ppInstructionName' :: String -> Doc OutputAnnot -> Doc OutputAnnot
ppInstructionName' name imod =
  indent 2 . annotate AnnInstruction $
    text name <> imod

ppOperand :: Operand64 -> Doc OutputAnnot
ppOperand = \case
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
ppRegister64 = \case
  RAX ->
    "%rax"
  RBX ->
    "%rbx"
  RCX ->
    "%rcx"
  RDX ->
    "%rdx"
  RBP ->
    "%rbp"
  RSP ->
    "%rsp"
  RSI ->
    "%rsi"
  RDI ->
    "%rdi"
  R8 ->
    "%r8"
  R9 ->
    "%r9"
  R10 ->
    "%r10"
  R11 ->
    "%r11"
  R12 ->
    "%r12"
  R13 ->
    "%r13"
  R14 ->
    "%r14"
  R15 ->
    "%r15"
  RFLAGS ->
    "%flags"

ppRegister8 :: Register64 -> Doc a
ppRegister8 = \case
  RAX ->
    "%al"
  RBX ->
    "%bl"
  RCX ->
    "%cl"
  RDX ->
    "%dl"
  RBP ->
    "%bpl"
  RSP ->
    "%spl"
  RSI ->
    "%sil"
  RDI ->
    "%dil"
  R8 ->
    "%r8b"
  R9 ->
    "%r9b"
  R10 ->
    "%r10b"
  R11 ->
    "%r11b"
  R12 ->
    "%r12b"
  R13 ->
    "%r13b"
  R14 ->
    "%r14b"
  R15 ->
    "%r15b"
  RFLAGS ->
    "%flags"

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
  X64.Movzbq ->
    text "movzbq"
  X64.Test ->
    text "test"
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
  G ->
    text "g"
  Ge ->
    text "ge"
