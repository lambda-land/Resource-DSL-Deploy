module TestParser where

import Prelude hiding (LT)

import Data.Monoid
import Data.Bifunctor (first)
import Control.Applicative (empty, liftA2)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T

data V a = One a | Chc Int (V a) (V a)
  deriving (Eq, Show)

data Expr =
    I Int
  | Neg (V Expr)
  | Add (V Expr) (V Expr)
  | Mul (V Expr) (V Expr)
  | Sub (V Expr) (V Expr)
  deriving (Eq, Show)

type Parser = Parsec Void T.Text

topLevel :: Parser a -> Parser a
topLevel p = between sc eof p

-- | Parse a Text value as an expression.
parseText :: Parser a -> T.Text -> Either String a
parseText p = first parseErrorPretty . parse (topLevel p) ""

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

rword :: T.Text -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar)

rws :: [T.Text]
rws = ["true", "false"]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

bool :: Parser Bool
bool = tru
    <|> fls
    <?> "boolean literal"
  where
    tru = True <$ rword "true"
    fls = False <$ rword "false"

int :: Parser Int
int = fmap fromInteger (lexeme L.decimal) <?> "integer literal"

name :: Parser Char -> Parser Char -> Parser T.Text
name start rest = lexeme (liftA2 T.cons start (T.pack <$> many rest))

var :: Parser T.Text
var = name start rest >>= check <?> "variable name"
  where
    start = char '$' <|> char '_' <|> letterChar
    rest  = char '$' <|> char '_' <|> alphaNumChar
    check x = if x `elem` rws
                then fail $ "Keyword " <> show x <> " cannot be a variable name."
                else return x

v :: Parser a -> Parser (V a)
v a = chc <|> one <?> "choice expression"
  where
    chc = do
      symbol "["
      d <- int
      symbol "]"
      symbol "{"
      l <- v a
      symbol ","
      r <- v a
      symbol "}"
      return (Chc d l r)
    one = One <$> a

eterm :: Parser Expr
eterm =
      parens expr
  <|> I <$> int
  <|> Neg <$ symbol "-" <*> v eterm

eop1 :: Parser (V Expr -> V Expr -> Expr)
eop1 =
      Add <$ symbol "+"
  <|> Sub <$ symbol "-"

pInfixL :: Parser (V Expr -> V Expr -> Expr) -> Parser (V Expr) -> V Expr -> Parser Expr
pInfixL op p x = do
  f <- op
  y <- p
  let r = f x y
  pInfixL op p (One r) <|> return r

expr1 :: Parser Expr
expr1 = try ((One <$> expr2) >>= pInfixL eop1 (One <$> expr2)) <|> eterm

eop2 :: Parser (V Expr -> V Expr -> Expr)
eop2 =
    Mul <$ symbol "*"

expr2 :: Parser Expr
expr2 = try (v eterm >>= pInfixL eop2 (v eterm)) <|> eterm

expr = expr1
