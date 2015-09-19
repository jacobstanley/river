{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module River.Source.Parser where

import           Control.Applicative (Applicative(..), Alternative(..), (<$>))
import           Control.Monad (MonadPlus(..))
import           Control.Monad.Trans.Either

import qualified Data.ByteString.UTF8 as UTF8
import           Data.Monoid (Monoid(..))
import qualified Data.Text as T

import           Text.Parser.Expression
import qualified Text.Parser.Token.Highlight as H
import           Text.Parser.Token.Style
import qualified Text.PrettyPrint.ANSI.Leijen as ANSI
import           Text.Trifecta
import           Text.Trifecta.Delta (Delta(..))
import           Text.Trifecta.Parser (parseFromFile)
import           Text.Trifecta.Result (Result(..))

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
pStatement = pDeclaration
         <|> pReturn
         <|> pAssignment
         <?> "statement"

pDeclaration :: RiverParser (Statement Delta)
pDeclaration = do
    pos   <- position
    pReserved "int"
    ident <- pIdentifier
    expr  <- try (pEquals *> (Just <$> pExpression)) <|> pure Nothing
    semi
    return (Declaration pos ident expr)

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
                 <*> (pReserved "return" *> pExpression) <* semi

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

pIdentifier :: (DeltaParsing m, TokenParsing m) => m Identifier
pIdentifier = token . highlight H.Identifier $ do
    x   <-       letter   <|> char '_'
    xs  <- many (alphaNum <|> char '_')
    return (Identifier (T.pack (x:xs)))

pReserved :: TokenParsing m => String -> m String
pReserved name = token (highlight H.ReservedIdentifier (string name))

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
commentStyle = CommentStyle "/*" "*/" "//" True
