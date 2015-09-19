{-# LANGUAGE RankNTypes #-}

module River.Source.Parser where

import           Control.Applicative ((<$>), (<|>), (<*>), (<*), (*>), pure)
import           Control.Monad.Trans.Either

import qualified Data.Text as T

import           Text.Parser.Expression
import qualified Text.Parser.Token.Highlight as H
import           Text.Parser.Token.Style
import qualified Text.PrettyPrint.ANSI.Leijen as ANSI
import           Text.Trifecta
import           Text.Trifecta.Delta (Delta)
import           Text.Trifecta.Parser (parseFromFile)
import           Text.Trifecta.Result (Result(..))

import           River.Source.Syntax

------------------------------------------------------------------------

data ParseError =
    TrifectaError ANSI.Doc
  deriving (Show)

parseProgram :: FilePath -> EitherT ParseError IO (Program Delta)
parseProgram path = EitherT $ do
  result <- parseFromFileEx pProgram path
  case result of
    Failure e -> return (Left (TrifectaError e))
    Success x -> return (Right x)

------------------------------------------------------------------------

type P a = (Monad m, TokenParsing m, DeltaParsing m) => m a

pProgram :: P (Program Delta)
pProgram = do
    whiteSpace
    pos <- position
    pReserved "int"
    pReserved "main"
    parens (pure ())
    braces (Program pos <$> many pStatement)

------------------------------------------------------------------------

pStatement :: P (Statement Delta)
pStatement = try pDeclaration
         <|> try pAssignment
         <|>     pReturn
         <?> "statement"

pDeclaration :: P (Statement Delta)
pDeclaration = do
    pos   <- position
    pReserved "int"
    ident <- pIdentifier
    expr  <- try (pEquals *> (Just <$> pExpression)) <|> pure Nothing
    semi
    return (Declaration pos ident expr)

pAssignment :: P (Statement Delta)
pAssignment = Assignment <$> position
                         <*> pIdentifier
                         <*> pAssignOp
                         <*> pExpression <* semi

pAssignOp :: P (Maybe BinaryOp)
pAssignOp = pOperator "="  *> pure Nothing
        <|> pOperator "+=" *> pure (Just Add)
        <|> pOperator "-=" *> pure (Just Sub)
        <|> pOperator "*=" *> pure (Just Mul)
        <|> pOperator "/=" *> pure (Just Div)
        <|> pOperator "%=" *> pure (Just Mod)

pReturn :: P (Statement Delta)
pReturn = Return <$> position
                 <*> (pReserved "return" *> pExpression) <* semi

------------------------------------------------------------------------

pExpression :: P (Expression Delta)
pExpression = buildExpressionParser opTable pExpression0 <?> "expression"

pExpression0 :: P (Expression Delta)
pExpression0 = parens (pExpression) <|> try pLiteral <|> pVariable

pLiteral :: P (Expression Delta)
pLiteral = Literal <$> position <*> integer

pVariable :: P (Expression Delta)
pVariable = Variable <$> position <*> pIdentifier

opTable :: DeltaParsing m => [[Operator m (Expression Delta)]]
opTable = [[prefix "-" (\p -> Unary  p Neg)],
           [binary "*" (\p -> Binary p Mul) AssocLeft,
            binary "/" (\p -> Binary p Div) AssocLeft,
            binary "%" (\p -> Binary p Mod) AssocLeft],
           [binary "+" (\p -> Binary p Add) AssocLeft,
            binary "-" (\p -> Binary p Sub) AssocLeft]]

------------------------------------------------------------------------

pIdentifier :: (DeltaParsing m, TokenParsing m) => m (Identifier Delta)
pIdentifier = token . highlight H.Identifier $ do
    pos <- position
    x   <-       letter   <|> char '_'
    xs  <- many (alphaNum <|> char '_')
    return (Identifier pos (T.pack (x:xs)))

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
