{-# LANGUAGE LambdaCase #-}

module River.Source.Pretty where

import qualified Data.Text as T

import           River.Source.Syntax

import           System.Console.ANSI

import           Text.PrettyPrint.Annotated.Leijen

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
displayProgram program = displayDecorated put doc
  where
    doc = renderPretty 0.8 80 (ppProgram program)

    put attr str = sgrAttr attr ++ str ++ sgrReset

    sgrReset = setSGRCode [Reset]

    sgrAttr = \case
      AnnLiteral     -> setSGRCode [SetColor Foreground Dull Red]
      AnnVariable    -> setSGRCode [SetColor Foreground Dull Green]
      AnnDeclaration -> setSGRCode [SetColor Foreground Dull Magenta]
      AnnAssignment  -> setSGRCode [SetColor Foreground Dull Cyan]
      AnnKeyword     -> setSGRCode [SetColor Foreground Dull Blue]
      AnnOperator    -> setSGRCode [SetColor Foreground Dull Yellow]

------------------------------------------------------------------------

ppProgram :: Program a -> Doc OutputAnnot
ppProgram = \case
  Program _ ff -> vcat [ ppKeyword "int" <+> annotate AnnDeclaration (text "main")
                                          <> text "() {"
                       , indent 4 (ppFragment ff)
                       , text "}" ]

ppFragment :: Fragment a -> Doc OutputAnnot
ppFragment = \case
  Declaration _ n mx ff
   -> let
          assignment = maybe empty (\x -> (empty <+> ppOperator "=" <+> ppExpression 0 x)) mx
      in
          ppKeyword "int" <+> annotate AnnDeclaration (ppIdentifier n)
                           <> assignment
                           <> semi
                         <$$> ppFragment ff

  Assignment _ n op x ff
   -> annotate AnnAssignment (ppIdentifier n) <+> ppAssignOp op
                                              <+> ppExpression 0 x
                                               <> semi
                                             <$$> ppFragment ff
  Return _ x
   -> ppKeyword "return" <+> ppExpression 0 x <> semi

ppExpression :: Prec -> Expression a -> Doc OutputAnnot
ppExpression p = \case
  Literal  _ i
   -> annotate AnnLiteral (integer i)

  Variable _ n
   -> annotate AnnVariable (ppIdentifier n)

  Unary _ op x
   | pop <- precUnaryOp op
   -> ppParens (p > pop)
               (ppUnaryOp op <> ppExpression (pop+1) x)

  Binary _ op x y
   | pop <- precBinaryOp op
   -> ppParens (p > pop)
               (ppExpression (pop+1) x <+> ppBinaryOp op <+> ppExpression (pop+1) y)

------------------------------------------------------------------------

ppIdentifier :: Identifier -> Doc OutputAnnot
ppIdentifier (Identifier n) = text (T.unpack n)

ppUnaryOp :: UnaryOp -> Doc OutputAnnot
ppUnaryOp = \case
  Neg -> ppOperator "-"

ppAssignOp :: Maybe BinaryOp -> Doc OutputAnnot
ppAssignOp Nothing   = ppOperator "="
ppAssignOp (Just op) = annotate AnnOperator (noAnnotate (ppBinaryOp op <> text "="))

ppBinaryOp :: BinaryOp -> Doc OutputAnnot
ppBinaryOp = \case
  Add -> ppOperator "+"
  Sub -> ppOperator "-"
  Mul -> ppOperator "*"
  Div -> ppOperator "/"
  Mod -> ppOperator "%"

ppKeyword :: String -> Doc OutputAnnot
ppKeyword = annotate AnnKeyword . text

ppOperator :: String -> Doc OutputAnnot
ppOperator = annotate AnnOperator . text

------------------------------------------------------------------------

type Prec = Int

ppParens :: Bool -> Doc a -> Doc a
ppParens True  = parens
ppParens False = id

precUnaryOp :: UnaryOp -> Prec
precUnaryOp = \case
  Neg -> 3

precBinaryOp :: BinaryOp -> Prec
precBinaryOp = \case
  Add -> 1
  Sub -> 1
  Mul -> 2
  Div -> 2
  Mod -> 2
