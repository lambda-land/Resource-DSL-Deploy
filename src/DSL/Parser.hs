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

import DSL.Types
import DSL.Expression
import DSL.Name
import DSL.Path
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

verbatim :: String -> Parser String
verbatim = L.symbol spaceConsumer

reserved :: String -> Parser ()
reserved w = lexeme (string w >> notFollowedBy alphaNumChar)


-- ** Expression Parser

unit :: Parser ()
unit = () <$ verbatim "()" <?> "unit literal"

bool :: Parser Bool
bool = tru <|> fls <?> "boolean literal"
  where
    tru = True <$ reserved "true"
    fls = False <$ reserved "false"

int :: Parser Int
int = fmap fromInteger (lexeme L.integer) <?> "integer literal"

float :: Parser Double
float = lexeme L.float <?> "floating point literal"

name :: Parser Char -> Parser Char -> Parser Name
name start rest = lexeme (liftA2 (:) start (many rest))

symbol :: Parser Symbol
symbol = char ':' >> Symbol <$> name start rest <?> "symbol"
  where
    start = char '_' <|> letterChar 
    rest  = char '_' <|> char '-' <|> alphaNumChar

var :: Parser Var
var = name start rest <?> "variable name"
  where
    start = char '$' <|> char '_' <|> letterChar
    rest  = char '$' <|> char '_' <|> alphaNumChar

path :: Parser Path
path = char '@' >> absolute <|> relative <?> "resource path"
  where
    absolute = char '/' >> Path Absolute <$> steps
    relative = Path Relative <$> steps
    steps = sepBy step (char '/')
    step = verbatim "." <|> verbatim ".." <|> many (char '_' <|> alphaNumChar)

parens :: Parser a -> Parser a
parens = between (lexeme (char '(')) (lexeme (char ')'))

opTable :: [[Operator Parser Expr]]
opTable =
    [ [op Prefix negate "-", op Prefix bnot "!"]
    , map inNN_N [Mul,Div,Mod]
    , map inNN_N [Add,Sub]
    , map inNN_B [LTE,LT,Equ,Neq,GTE,GT]
    , [inBB_B And]
    , map inBB_B [Or,XOr]
    , [inBB_B Imp]
    , [inBB_B Eqv]
    ]
  where
    op fixity f name = fixity (f <$ verbatim name)
    inNN_N o = op InfixL (opNN_N o) (prettyNN_N o)
    inNN_B o = op InfixN (opNN_B o) (prettyNN_B o)
    inBB_B o = op InfixR (opBB_B o) (prettyBB_B o)

expr :: Parser Expr
expr = do
    c <- simple
    option c $ do
      lexeme (char '?')
      t <- simple
      lexeme (char ':')
      e <- simple
      return (P3 Cond c t e)
  where
    simple = makeExprParser term opTable
    term = Lit Unit <$ unit
       <|> try (fmap (Lit . F) float)
       <|> fmap (Lit . I) int
       <|> fmap (Lit . S) symbol
       <|> try (fmap (Lit . B) bool)
       <|> try primFun
       <|> fmap Ref var
       <|> fmap Res path
       <|> parens expr

primFun :: Parser Expr
primFun = fun "unit"    U_U
      <|> fun "abs"     (N_N Abs)
      <|> fun "signum"  (N_N Sign)
      <|> fun "ceiling" (F_I Ceil)
      <|> fun "floor"   (F_I Floor)
      <|> fun "round"   (F_I Round)
  where
    fun name op = do
      reserved name
      P1 op <$> parens expr
