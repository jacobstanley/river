{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module River.Source.Concrete.Parser (
    parseProgram
  , parseProgram'
  ) where

import           Control.Applicative (Alternative(..))
import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (ExceptT(..))

import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Text.Megaparsec ((<?>))
import           Text.Megaparsec (ParseError, Dec, runParser)
import           Text.Megaparsec (SourcePos, getPosition)
import           Text.Megaparsec (label, try, optional, between, oneOf)
import           Text.Megaparsec (eof, spaceChar, letterChar, alphaNumChar, char, char')
import           Text.Megaparsec.Expr (Operator(..), makeExprParser)
import qualified Text.Megaparsec.Lexer as Lexer
import           Text.Megaparsec.Prim (MonadParsec, Token)

import           River.Source.Concrete.Syntax

------------------------------------------------------------------------

parseProgram :: FilePath -> ExceptT (ParseError Char Dec) IO (Program SourcePos)
parseProgram path =
  ExceptT $ do
    source <- liftIO $ T.readFile path
    pure $ runParser pProgram path source

parseProgram' :: FilePath -> String -> Either (ParseError Char Dec) (Program SourcePos)
parseProgram' name source =
  runParser pProgram name source

------------------------------------------------------------------------

pProgram :: RiverParser s m => m (Program SourcePos)
pProgram = do
  pSpace
  pReserved "int"

  pos <- getPosition
  pReserved "main"
  pParens $ pure ()

  Program pos <$> pBlock <* eof

------------------------------------------------------------------------

pBlock :: RiverParser s m => m (Block SourcePos)
pBlock = do
  Block
    <$> getPosition
    <*> pBraces pStatements

pStatements :: RiverParser s m => m [Statement SourcePos]
pStatements =
  many pStatement

pStatement :: RiverParser s m => m (Statement SourcePos)
pStatement =
  (SSimple <$> getPosition <*> pSimple <* pSemi <?> "simple statement") <|>
  (SControl <$> getPosition <*> pControl <?> "control statement") <|>
  (SBlock <$> getPosition <*> pBlock <?> "block")

pSimple :: RiverParser s m => m (Simple SourcePos)
pSimple =
  (pAssignPost <?> "assignment or postfix operation") <|>
  (pDeclare <?> "declaration")

pAssignPost :: RiverParser s m => m (Simple SourcePos)
pAssignPost = do
  pos <- getPosition
  lv <- pLValue
  pAssign pos lv <|> pPost pos lv

pAssign :: RiverParser s m => SourcePos -> LValue SourcePos -> m (Simple SourcePos)
pAssign pos lv =
  Assign pos lv
    <$> pAssignOp
    <*> pExpression

pPost :: RiverParser s m => SourcePos -> LValue SourcePos -> m (Simple SourcePos)
pPost pos lv =
  Post pos lv
    <$> pPostOp

pDeclare :: RiverParser s m => m (Simple SourcePos)
pDeclare =
  Declare
    <$> getPosition
    <*> pType
    <*> pIdentifier
    <*> optional (pEquals *> pExpression)

pControl :: RiverParser s m => m (Control SourcePos)
pControl =
  (pIf <?> "if statement") <|>
  (pWhile <?> "while loop") <|>
  (pFor <?> "for loop") <|>
  (pReturn <?> "return")

pIf :: RiverParser s m => m (Control SourcePos)
pIf =
  let
    pElse =
      pReserved "else" *> pStatement
  in
    If
      <$> (getPosition <* pReserved "if")
      <*> pParens pExpression
      <*> pStatement
      <*> optional pElse

pWhile :: RiverParser s m => m (Control SourcePos)
pWhile =
  While
    <$> (getPosition <* pReserved "while")
    <*> pParens pExpression
    <*> pStatement

pFor :: RiverParser s m => m (Control SourcePos)
pFor =
  For
    <$> (getPosition <* pReserved "for" <* pSymbol "(")
    <*> (optional pSimple <* pSemi)
    <*> (pExpression <* pSemi)
    <*> (optional pSimple <* pSymbol ")")
    <*> pStatement

pAssignOp :: RiverParser s m => m AssignOp
pAssignOp =
  pReservedOp "="  *> pure AEq <|>
  pReservedOp "+=" *> pure AAdd <|>
  pReservedOp "-=" *> pure ASub <|>
  pReservedOp "*=" *> pure AMul <|>
  pReservedOp "/=" *> pure ADiv <|>
  pReservedOp "%=" *> pure AMod <|>
  pReservedOp "<<=" *> pure AShl <|>
  pReservedOp ">>=" *> pure AShr <|>
  pReservedOp "&=" *> pure AAnd <|>
  pReservedOp "^=" *> pure AXor <|>
  pReservedOp "|=" *> pure AOr

pPostOp :: RiverParser s m => m PostOp
pPostOp =
  pReservedOp "++" *> pure Inc <|>
  pReservedOp "--" *> pure Dec

pReturn :: RiverParser s m => m (Control SourcePos)
pReturn =
  Return
    <$> getPosition
    <*> (pReserved "return" *> pExpression) <* pSemi

------------------------------------------------------------------------

pExpression :: RiverParser s m => m (Expression SourcePos)
pExpression =
  try pConditional <|>
  pExpression0

pConditional :: RiverParser s m => m (Expression SourcePos)
pConditional =
  Conditional
    <$> getPosition
    <*> pExpression0 <* pReservedOp "?"
    <*> pExpression  <* pReservedOp ":"
    <*> pExpression

pExpression0 :: RiverParser s m => m (Expression SourcePos)
pExpression0 =
  makeExprParser pExpression1 opTable

pExpression1 :: RiverParser s m => m (Expression SourcePos)
pExpression1 =
  pParens (pExpression) <|>
  try pLiteral <|>
  pVariable

pLiteral :: RiverParser s m => m (Expression SourcePos)
pLiteral =
  Literal
    <$> getPosition
    <*> (pLiteralInt <|> pLiteralTrue <|> pLiteralFalse)
    <?> "literal"

pLiteralInt :: RiverParser s m => m Literal
pLiteralInt =
  pLiteralHexZero <|>
  pLiteralDec

pLiteralDec :: RiverParser s m => m Literal
pLiteralDec =
  LiteralInt <$> pLexeme Lexer.decimal

pLiteralHexZero :: RiverParser s m => m Literal
pLiteralHexZero =
  char '0' *> (pLiteralHex <|> pLiteralZero)

pLiteralZero :: RiverParser s m => m Literal
pLiteralZero =
  LiteralInt 0 <$ pSpace

pLiteralHex :: RiverParser s m => m Literal
pLiteralHex =
  LiteralInt <$> (char' 'x' *> pLexeme Lexer.hexadecimal)

pLiteralTrue :: RiverParser s m => m Literal
pLiteralTrue =
  LiteralBool <$> (True <$ pReserved "true") <?> "true"

pLiteralFalse :: RiverParser s m => m Literal
pLiteralFalse =
  LiteralBool <$> (False <$ pReserved "false") <?> "false"

pVariable :: RiverParser s m => m (Expression SourcePos)
pVariable =
  Variable <$> getPosition <*> pIdentifier <?> "variable"

------------------------------------------------------------------------
-- Operator Table

opTable :: RiverParser s m => [[Operator m (Expression SourcePos)]]
opTable =
  [ [ prefix "!"  (\p -> Unary p LNot)
    , prefix "~"  (\p -> Unary p BNot)
    , prefix "-"  (\p -> Unary p Neg)
    ]
  , [ infixL "*"  (\p -> Binary p Mul)
    , infixL "/"  (\p -> Binary p Div)
    , infixL "%"  (\p -> Binary p Mod)
    ]
  , [ infixL "+"  (\p -> Binary p Add)
    , infixL "-"  (\p -> Binary p Sub)
    ]
  , [ infixL "<<" (\p -> Binary p Shl)
    , infixL ">>" (\p -> Binary p Shr)
    ]
  , [ infixL "<"  (\p -> Binary p Lt)
    , infixL "<=" (\p -> Binary p Le)
    , infixL ">"  (\p -> Binary p Gt)
    , infixL ">=" (\p -> Binary p Ge)
    ]
  , [ infixL "==" (\p -> Binary p Eq)
    , infixL "!=" (\p -> Binary p Ne)
    ]
  , [ infixL "&"  (\p -> Binary p BAnd)
    ]
  , [ infixL "^"  (\p -> Binary p BXor)
    ]
  , [ infixL "|"  (\p -> Binary p BOr)
    ]
  , [ infixL "&&" (\p -> Binary p LAnd)
    ]
  , [ infixL "||" (\p -> Binary p LOr)
    ]
  ]

infixL :: RiverParser s m => String -> (SourcePos -> a -> a -> a) -> Operator m a
infixL name fun =
  InfixL (fmap fun (getPosition <* pReservedOp name))

prefix :: RiverParser s m => String -> (SourcePos -> a -> a) -> Operator m a
prefix name fun =
  Prefix (fmap fun (getPosition <* pReservedOp name))

------------------------------------------------------------------------

pType :: RiverParser s m => m Type
pType =
  (Int <$ pReserved "int") <|>
  (Bool <$ pReserved "bool") <?> "type"

pLValue :: RiverParser s m => m (LValue SourcePos)
pLValue =
  LIdentifier
    <$> getPosition
    <*> pIdentifier

pIdentifier :: RiverParser s m => m Identifier
pIdentifier =
  try . label "identifier" $ do
    parsed <- pIdent
    if Set.member parsed reservedIdents then
      fail $ "keyword '" ++ parsed ++ "' cannot be an identifier"
    else
      pure . Identifier $ T.pack parsed

pReserved :: RiverParser s m => String -> m ()
pReserved expected =
  try . label expected $ do
    parsed <- pIdent
    if parsed == expected then
      pure ()
    else
      fail $ "expected keyword '" ++ expected ++ "'"

pReservedOp :: RiverParser s m => String -> m ()
pReservedOp expected =
  try . label expected $ do
    parsed <- pOperator
    if parsed == expected then
      pure ()
    else
      fail $ "expected operator " ++ expected

pEquals :: RiverParser s m => m ()
pEquals =
  pReservedOp "="

------------------------------------------------------------------------
-- Lexer

type RiverParser s m =
  (MonadParsec Dec s m, Token s ~ Char)

reservedIdents :: Set String
reservedIdents =
  Set.fromList
    [ "struct"
    , "typedef"
    , "if"
    , "else"
    , "while"
    , "for"
    , "continue"
    , "break"
    , "return"
    , "assert"
    , "true"
    , "false"
    , "NULL"
    , "alloc"
    , "alloc_array"
    , "int"
    , "bool"
    , "void"
    , "char"
    , "string"
    ]

pSpace :: RiverParser s m => m ()
pSpace =
  let
    lineComment =
      Lexer.skipLineComment "//"

    blockComment =
      Lexer.skipBlockComment "/*" "*/"
  in
    Lexer.space (void spaceChar) lineComment blockComment

pLexeme :: RiverParser s m => m a -> m a
pLexeme =
  Lexer.lexeme pSpace

pSymbol :: RiverParser s m => String -> m String
pSymbol =
  Lexer.symbol pSpace

pParens :: RiverParser s m => m a -> m a
pParens =
  between (pSymbol "(") (pSymbol ")")

pBraces :: RiverParser s m => m a -> m a
pBraces =
  between (pSymbol "{") (pSymbol "}")

pSemi :: RiverParser s m => m ()
pSemi =
  () <$ pSymbol ";"

pIdent :: RiverParser s m => m String
pIdent =
  pLexeme $
    (:) <$> pIdentStart <*> many pIdentLetter

pIdentStart :: RiverParser s m => m Char
pIdentStart =
  letterChar <|> char '_'

pIdentLetter :: RiverParser s m => m Char
pIdentLetter =
  alphaNumChar <|> char '_'

pOperator :: RiverParser s m => m String
pOperator =
  pLexeme $
    (:) <$> pOpStart <*> many pOpLetter

pOpStart :: RiverParser s m => m Char
pOpStart =
  oneOf "!~-+*/%<>&^|=?:"

pOpLetter :: RiverParser s m => m Char
pOpLetter =
  oneOf "-+<>&|="
