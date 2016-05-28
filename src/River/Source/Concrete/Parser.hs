{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
module River.Source.Concrete.Parser (
    ParseError(..)
  , parseProgram
  , parseProgram'

  , reservedNames

  , fileOfDelta
  , lineOfDelta
  , columnOfDelta
  ) where

import           Control.Applicative (Alternative(..))
import           Control.Monad (MonadPlus(..))
import           Control.Monad.Trans.Except (ExceptT(..))

import qualified Data.ByteString.UTF8 as UTF8
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.Text as T

import           Text.Parser.Expression (Operator(..), Assoc(..))
import           Text.Parser.Expression (buildExpressionParser)
import qualified Text.Parser.Token.Highlight as Highlight
import           Text.Parser.Token.Style (CommentStyle(..))
import           Text.Parser.Token.Style (buildSomeSpaceParser)
import qualified Text.PrettyPrint.ANSI.Leijen as ANSI
import           Text.Trifecta ((<?>))
import           Text.Trifecta (CharParsing(..), TokenParsing(..))
import           Text.Trifecta (IdentifierStyle(..))
import           Text.Trifecta (MarkParsing(..), DeltaParsing(..))
import           Text.Trifecta (Parsing(..), Parser(..), Result(..))
import           Text.Trifecta (letter, alphaNum, whiteSpace)
import           Text.Trifecta (reserve, ident, natural, symbolic)
import           Text.Trifecta (parens, braces, optional)
import qualified Text.Trifecta as Trifecta
import           Text.Trifecta.Delta (Delta(..))
import qualified Text.Trifecta.Delta as Trifecta

import           River.Source.Concrete.Syntax

------------------------------------------------------------------------

data ParseError =
    TrifectaError ANSI.Doc
    deriving (Show)

parseProgram :: FilePath -> ExceptT ParseError IO (Program Delta)
parseProgram path =
  ExceptT $ do
    result <- Trifecta.parseFromFileEx (runRiverParser pProgram) path
    case result of
      Failure e ->
        pure . Left $ TrifectaError e
      Success x ->
        pure $ Right x

parseProgram' :: FilePath -> String -> Either ParseError (Program Delta)
parseProgram' name source =
  let
    delta =
      Directed (UTF8.fromString name) 0 0 0 0
  in
    case Trifecta.parseString (runRiverParser pProgram) delta source of
      Failure e ->
        Left $ TrifectaError e
      Success x ->
        Right x

------------------------------------------------------------------------

pProgram :: RiverParser (Program Delta)
pProgram = do
  whiteSpace
  pReserved "int"

  pos <- position
  pReserved "main"
  parens $ pure ()

  Program pos <$> pBlock <* eof

------------------------------------------------------------------------

pBlock :: RiverParser (Block Delta)
pBlock = do
  Block
    <$> position
    <*> braces pStatements

pStatements :: RiverParser [Statement Delta]
pStatements =
  many pStatement

pStatement :: RiverParser (Statement Delta)
pStatement =
  (try $ SSimple <$> position <*> pSimple <* semi <?> "simple statement") <|>
  (try $ SControl <$> position <*> pControl <?> "control statement") <|>
  (SBlock <$> position <*> pBlock <?> "block")

pSimple :: RiverParser (Simple Delta)
pSimple =
  (try $ pAssign <?> "assignment") <|>
  (try $ pPost <?> "post operation") <|>
  (pDeclare <?> "declaration")

pAssign :: RiverParser (Simple Delta)
pAssign =
  Assign
    <$> position
    <*> pLValue
    <*> pAssignOp
    <*> pExpression

pPost :: RiverParser (Simple Delta)
pPost =
  Post
    <$> position
    <*> pLValue
    <*> pPostOp

pDeclare :: RiverParser (Simple Delta)
pDeclare =
  Declare
    <$> position
    <*> pType
    <*> pIdentifier
    <*> optional (pEquals *> pExpression)

pControl :: RiverParser (Control Delta)
pControl =
  (try $ pIf <?> "if statement") <|>
  (try $ pWhile <?> "while loop") <|>
  (try $ pFor <?> "for loop") <|>
  (pReturn <?> "return")

pIf :: RiverParser (Control Delta)
pIf =
  let
    pElse =
      pReserved "else" *> pStatement
  in
    If
      <$> (position <* pReserved "if")
      <*> parens pExpression
      <*> pStatement
      <*> optional pElse

pWhile :: RiverParser (Control Delta)
pWhile =
  While
    <$> (position <* pReserved "while")
    <*> parens pExpression
    <*> pStatement

pFor :: RiverParser (Control Delta)
pFor =
  For
    <$> (position <* pReserved "for" <* symbolic '(')
    <*> (optional pSimple <* semi)
    <*> (pExpression <* semi)
    <*> (optional pSimple <* symbolic ')')
    <*> pStatement

pAssignOp :: RiverParser AssignOp
pAssignOp =
  pOperator "="  *> pure AEq <|>
  pOperator "+=" *> pure AAdd <|>
  pOperator "-=" *> pure ASub <|>
  pOperator "*=" *> pure AMul <|>
  pOperator "/=" *> pure ADiv <|>
  pOperator "%=" *> pure AMod <|>
  pOperator "<<=" *> pure AShl <|>
  pOperator ">>=" *> pure AShr <|>
  pOperator "&=" *> pure AAnd <|>
  pOperator "^=" *> pure AXor <|>
  pOperator "|=" *> pure AOr

pPostOp :: RiverParser PostOp
pPostOp =
  pOperator "++" *> pure Inc <|>
  pOperator "--" *> pure Dec

pReturn :: RiverParser (Control Delta)
pReturn =
  Return
    <$> position
    <*> (pReserved "return" *> pExpression) <* semi

------------------------------------------------------------------------

pExpression :: RiverParser (Expression Delta)
pExpression =
  try pConditional <|> pExpression0

pConditional :: RiverParser (Expression Delta)
pConditional =
  Conditional
    <$> position
    <*> pExpression0 <* pOperator "?"
    <*> pExpression  <* pOperator ":"
    <*> pExpression

pExpression0 :: RiverParser (Expression Delta)
pExpression0 =
  buildExpressionParser opTable pExpression1 <?> "expression"

pExpression1 :: RiverParser (Expression Delta)
pExpression1 =
  parens (pExpression) <|>
  try pLiteral <|>
  pVariable

pLiteral :: RiverParser (Expression Delta)
pLiteral =
  Literal
    <$> position
    <*> (pLiteralInt <|> pLiteralTrue <|> pLiteralFalse)
    <?> "literal"

pLiteralInt :: RiverParser Literal
pLiteralInt =
  LiteralInt <$> natural <?> "integer"

pLiteralTrue :: RiverParser Literal
pLiteralTrue =
  LiteralBool <$> (True <$ pReserved "true") <?> "true"

pLiteralFalse :: RiverParser Literal
pLiteralFalse =
  LiteralBool <$> (False <$ pReserved "false") <?> "false"

pVariable :: RiverParser (Expression Delta)
pVariable =
  Variable <$> position <*> pIdentifier <?> "variable"

opTable :: DeltaParsing m => [[Operator m (Expression Delta)]]
opTable =
  [ [ prefix "!"  (\p -> Unary p LNot)
    , prefix "~"  (\p -> Unary p BNot)
    , prefix "-"  (\p -> Unary p Neg)
    ]
  , [ binary "*"  (\p -> Binary p Mul) AssocLeft
    , binary "/"  (\p -> Binary p Div) AssocLeft
    , binary "%"  (\p -> Binary p Mod) AssocLeft
    ]
  , [ binary "+"  (\p -> Binary p Add) AssocLeft
    , binary "-"  (\p -> Binary p Sub) AssocLeft
    ]
  , [ binary "<<" (\p -> Binary p Shl) AssocLeft
    , binary ">>" (\p -> Binary p Shr) AssocLeft
    ]
  , [ binary "<"  (\p -> Binary p Lt) AssocLeft
    , binary "<=" (\p -> Binary p Le) AssocLeft
    , binary ">"  (\p -> Binary p Gt) AssocLeft
    , binary ">=" (\p -> Binary p Ge) AssocLeft
    ]
  , [ binary "==" (\p -> Binary p Eq) AssocLeft
    , binary "!=" (\p -> Binary p NEq) AssocLeft
    ]
  , [ binary "&"  (\p -> Binary p BAnd) AssocLeft
    ]
  , [ binary "^"  (\p -> Binary p BXor) AssocLeft
    ]
  , [ binary "|"  (\p -> Binary p BOr) AssocLeft
    ]
  , [ binary "&&" (\p -> Binary p LAnd) AssocLeft
    ]
  , [ binary "||" (\p -> Binary p LOr) AssocLeft
    ]
  ]

------------------------------------------------------------------------

pType :: RiverParser Type
pType =
  (Int <$ pReserved "int") <|>
  (Bool <$ pReserved "bool") <?> "type"

pLValue :: RiverParser (LValue Delta)
pLValue =
  LIdentifier
    <$> position
    <*> pIdentifier

pIdentifier :: (Monad m, TokenParsing m) => m Identifier
pIdentifier =
  Identifier . T.pack <$> ident identStyle <?> "identifier"

pReserved :: (Monad m, TokenParsing m) => String -> m ()
pReserved =
  reserve identStyle

pOperator :: (Monad m, TokenParsing m) => String -> m ()
pOperator =
  reserve opStyle

pEquals :: (Monad m, TokenParsing m) => m ()
pEquals =
  pOperator "="

------------------------------------------------------------------------
-- Operator Table

binary ::
  DeltaParsing m =>
  String -> (Delta -> a -> a -> a) -> Assoc -> Operator m a
binary name fun assoc =
  Infix (fmap fun (position <* pOperator name)) assoc

prefix ::
  DeltaParsing m =>
  String -> (Delta -> a -> a) -> Operator m a
prefix name fun =
  Prefix (fmap fun (position <* pOperator name))

------------------------------------------------------------------------
-- Parser Monad

newtype RiverParser a =
  RiverParser {
      runRiverParser :: Parser a
    } deriving (
      Monoid
    , Functor
    , Applicative
    , Alternative
    , Monad
    , MonadPlus
    , CharParsing
    , DeltaParsing
    , MarkParsing Delta
    )

deriving instance Parsing RiverParser

instance TokenParsing RiverParser where
  someSpace =
    RiverParser $ buildSomeSpaceParser someSpace commentStyle

  nesting =
    RiverParser . nesting . runRiverParser

  highlight h =
    RiverParser . highlight h . runRiverParser

  semi =
    token (char ';' <?> ";")

  token p =
    p <* whiteSpace

commentStyle :: CommentStyle
commentStyle =
  CommentStyle {
      _commentStart =
        "/*"

    , _commentEnd =
        "*/"

    , _commentLine =
        "//"

    , _commentNesting =
        True
    }

identStyle :: CharParsing m => IdentifierStyle m
identStyle =
  IdentifierStyle {
      _styleName =
        "identifier"

    , _styleStart =
        pIdentStart

    , _styleLetter =
        pIdentLetter

    , _styleReserved =
        reservedNames

    , _styleHighlight =
        Highlight.Identifier

    , _styleReservedHighlight =
        Highlight.ReservedIdentifier
    }

reservedNames :: HashSet String
reservedNames =
  HashSet.fromList
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

pIdentStart :: CharParsing m => m Char
pIdentStart =
  letter <|> char '_'

pIdentLetter :: CharParsing m => m Char
pIdentLetter =
  alphaNum <|> char '_'

opStyle :: CharParsing m => IdentifierStyle m
opStyle =
  IdentifierStyle {
      _styleName =
        "operator"

    , _styleStart =
        pOpStart

    , _styleLetter =
        pOpLetter

    , _styleReserved =
        HashSet.empty

    , _styleHighlight =
        Highlight.Operator

    , _styleReservedHighlight =
        Highlight.ReservedOperator
    }

pOpStart :: CharParsing m => m Char
pOpStart =
  Trifecta.oneOf "!~-*/%+<>&^|=?:"

pOpLetter :: CharParsing m => m Char
pOpLetter =
  Trifecta.oneOf "<>&|="

------------------------------------------------------------------------
-- Delta

fileOfDelta :: Delta -> FilePath
fileOfDelta = \case
  Columns _ _->
    "(interactive)"
  Tab _ _ _ ->
    "(interactive)"
  Lines _ _ _ _ ->
    "(interactive)"
  Directed fn _ _ _ _ ->
    UTF8.toString fn

lineOfDelta :: Delta -> Int
lineOfDelta = \case
  Columns _ _->
    0
  Tab _ _ _ ->
    0
  Lines l _ _ _ ->
    fromIntegral l + 1
  Directed _ l _ _ _ ->
    fromIntegral l + 1

columnOfDelta :: Delta -> Int
columnOfDelta pos =
  fromIntegral (Trifecta.column pos) + 1
