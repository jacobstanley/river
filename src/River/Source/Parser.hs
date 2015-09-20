{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module River.Source.Parser where

import           Control.Applicative (Applicative(..), Alternative(..), (<$>))
import           Control.Monad (MonadPlus(..))
import           Control.Monad.Trans.Either

import qualified Data.ByteString.UTF8 as UTF8
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Monoid (Monoid(..))
import qualified Data.Text as T

import           Text.Parser.Expression
import qualified Text.Parser.Token.Highlight as H
import           Text.Parser.Token.Style
import qualified Text.PrettyPrint.ANSI.Leijen as ANSI
import           Text.Trifecta hiding (source)
import           Text.Trifecta.Delta (Delta(..), column)

import           River.Source.Syntax

------------------------------------------------------------------------

data ParseError =
    TrifectaError ANSI.Doc
  deriving (Show)

------------------------------------------------------------------------

parseProgram :: FilePath -> EitherT ParseError IO (Program Delta)
parseProgram path = EitherT $ do
  result <- parseFromFileEx (runRiverParser pProgram) path
  case result of
    Failure e -> return (Left (TrifectaError e))
    Success x -> return (Right x)

parseProgramFromString :: FilePath -> String -> Either ParseError (Program Delta)
parseProgramFromString name source =
  let delta = Directed (UTF8.fromString name) 0 0 0 0
  in case parseString (runRiverParser pProgram) delta source of
    Failure e -> Left (TrifectaError e)
    Success x -> Right x

------------------------------------------------------------------------

pProgram :: RiverParser (Program Delta)
pProgram = do
    whiteSpace
    pos <- position
    pReserved "int"
    pReserved "main"
    parens (pure ())
    braces (Program pos <$> many pStatement)

------------------------------------------------------------------------

pStatement :: RiverParser (Statement Delta)
pStatement = (pDeclaration <?> "declaration")
         <|> (pAssignment  <?> "assignment")
         <|> (pReturn      <?> "return")

pDeclaration :: RiverParser (Statement Delta)
pDeclaration = do
    pos  <- position
    try (pReserved "int")
    name <- pIdentifier
    expr <- (pEquals *> (Just <$> pExpression)) <|> pure Nothing
    _    <- semi
    return (Declaration pos name expr)

pAssignment :: RiverParser (Statement Delta)
pAssignment = Assignment <$> position
                         <*> pIdentifier
                         <*> pAssignOp
                         <*> pExpression <* semi

pAssignOp :: RiverParser (Maybe BinaryOp)
pAssignOp = pOperator "="  *> pure Nothing
        <|> pOperator "+=" *> pure (Just Add)
        <|> pOperator "-=" *> pure (Just Sub)
        <|> pOperator "*=" *> pure (Just Mul)
        <|> pOperator "/=" *> pure (Just Div)
        <|> pOperator "%=" *> pure (Just Mod)

pReturn :: RiverParser (Statement Delta)
pReturn = Return <$> position
                 <*> (try (pReserved "return") *> pExpression) <* semi

------------------------------------------------------------------------

pExpression :: RiverParser (Expression Delta)
pExpression = buildExpressionParser opTable pExpression0 <?> "expression"

pExpression0 :: RiverParser (Expression Delta)
pExpression0 = parens (pExpression) <|> try pLiteral <|> pVariable

pLiteral :: RiverParser (Expression Delta)
pLiteral = Literal <$> position <*> natural <?> "literal"

pVariable :: RiverParser (Expression Delta)
pVariable = Variable <$> position <*> pIdentifier <?> "variable"

opTable :: DeltaParsing m => [[Operator m (Expression Delta)]]
opTable = [[prefix "-" (\p -> Unary  p Neg)],
           [binary "*" (\p -> Binary p Mul) AssocLeft,
            binary "/" (\p -> Binary p Div) AssocLeft,
            binary "%" (\p -> Binary p Mod) AssocLeft],
           [binary "+" (\p -> Binary p Add) AssocLeft,
            binary "-" (\p -> Binary p Sub) AssocLeft]]

------------------------------------------------------------------------

pIdentifier :: (Monad m, TokenParsing m) => m Identifier
pIdentifier = Identifier . T.pack <$> ident identStyle <?> "identifier"

pReserved :: (Monad m, TokenParsing m) => String -> m ()
pReserved = reserve identStyle

pOperator :: TokenParsing m => String -> m String
pOperator name = token (highlight H.Operator (string name))

pEquals :: TokenParsing m => m ()
pEquals = pOperator "=" *> pure ()

------------------------------------------------------------------------
-- Operator Table

binary :: DeltaParsing m => String -> (Delta -> a -> a -> a) -> Assoc -> Operator m a
prefix :: DeltaParsing m => String -> (Delta -> a -> a)               -> Operator m a

binary  name fun assoc = Infix  (fun <$> (position <* pOperator name)) assoc
prefix  name fun       = Prefix (fun <$> (position <* pOperator name))

------------------------------------------------------------------------
-- Parser Monad

newtype RiverParser a = RiverParser { runRiverParser :: Parser a }
  deriving ( Monoid
           , Functor
           , Applicative
           , Alternative
           , Monad
           , MonadPlus
           , CharParsing
           , DeltaParsing
           , MarkParsing Delta )

deriving instance Parsing RiverParser

instance TokenParsing RiverParser where
  someSpace   = RiverParser (buildSomeSpaceParser someSpace commentStyle)
  nesting     = RiverParser . nesting . runRiverParser
  highlight h = RiverParser . highlight h . runRiverParser
  semi        = token (char ';' <?> ";")
  token p     = p <* whiteSpace

commentStyle :: CommentStyle
commentStyle = CommentStyle
  { _commentStart   = "/*"
  , _commentEnd     = "*/"
  , _commentLine    = "//"
  , _commentNesting = True
  }

identStyle :: CharParsing m => IdentifierStyle m
identStyle = IdentifierStyle
  { _styleName              = "identifier"
  , _styleStart             = pIdentStart
  , _styleLetter            = pIdentLetter
  , _styleReserved          = reservedNames
  , _styleHighlight         = H.Identifier
  , _styleReservedHighlight = H.ReservedIdentifier
  }

reservedNames :: HashSet String
reservedNames = HashSet.fromList
  [ "struct", "typedef"
  , "if", "else", "while", "for"
  , "continue", "break", "return"
  , "assert"
  , "true", "false", "NULL"
  , "alloc", "alloc_array"
  , "int", "bool", "void", "char", "string"
  ]

pIdentStart :: CharParsing m => m Char
pIdentStart = letter <|> char '_'

pIdentLetter :: CharParsing m => m Char
pIdentLetter = alphaNum <|> char '_'

------------------------------------------------------------------------
-- Delta

fileOfDelta :: Delta -> FilePath
fileOfDelta (Directed fn _ _ _ _) = UTF8.toString fn
fileOfDelta _                     = "(interactive)"

lineOfDelta :: Delta -> Int
lineOfDelta (Lines l _ _ _)      = fromIntegral l + 1
lineOfDelta (Directed _ l _ _ _) = fromIntegral l + 1
lineOfDelta _                    = 0

columnOfDelta :: Delta -> Int
columnOfDelta pos = fromIntegral (column pos) + 1
