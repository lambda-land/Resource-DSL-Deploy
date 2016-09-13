module DSL.Parser
  ( parseExprText, parseExprString
  ) where

import Prelude hiding (LT,GT)

import Control.Applicative (empty,liftA2)
import Control.Monad (void)

import Data.Bifunctor (first)
import Data.Text (Text,pack)

import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L

import DSL.Environment
import DSL.Expression
import DSL.Pretty
import DSL.Primitive


--
-- * Parse Expressions
--

-- | Parse a Text value as an expression.
parseExprText :: Text -> Either String Expr
parseExprText = first parseErrorPretty . parse (expr <* eof) ""

-- | Parse a String value as an expression.
parseExprString :: String -> Either String Expr
parseExprString = parseExprText . pack


--
-- * Internal
--

-- ** Lexer

spaceConsumer :: Parser ()
spaceConsumer = L.space (void spaceChar) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

reserved :: String -> Parser ()
reserved w = lexeme (string w >> notFollowedBy alphaNumChar)


-- ** Expression Parser

unit :: Parser ()
unit = () <$ symbol "()" <?> "unit literal"

bool :: Parser Bool
bool = tru <|> fls <?> "boolean literal"
  where
    tru = True <$ reserved "true"
    fls = False <$ reserved "false"

int :: Parser Int
int = fmap fromInteger (lexeme L.integer) <?> "integer literal"

var :: Parser Var
var = lexeme (liftA2 (:) varStart (many varRest)) <?> "variable name"
  where
    varStart = char '$' <|> char '_' <|> letterChar
    varRest  = char '$' <|> char '_' <|> alphaNumChar

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

opTable :: [[Operator Parser Expr]]
opTable =
    [ [op Prefix negate "-", op Prefix bnot "!"]
    , map inII_I [Mul,Div,Mod]
    , map inII_I [Add,Sub]
    , map inII_B [LTE,LT,Equ,Neq,GTE,GT]
    , [inBB_B And]
    , map inBB_B [Or,XOr]
    , [inBB_B Imp]
    , [inBB_B Eqv]
    ]
  where
    op fixity f name = fixity (f <$ symbol name)
    inII_I o = op InfixL (opII_I o) (pII_I o)
    inII_B o = op InfixN (opII_B o) (pII_B o)
    inBB_B o = op InfixR (opBB_B o) (pBB_B o)

expr :: Parser Expr
expr = makeExprParser term opTable <?> "expression"
  where
    term = Lit Unit <$ unit
       <|> fmap (Lit . I) int
       <|> try (fmap (Lit . B) bool)
       <|> fmap Ref var
       <|> parens expr
