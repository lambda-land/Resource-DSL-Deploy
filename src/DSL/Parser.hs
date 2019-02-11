module DSL.Parser where

import Prelude hiding (LT,GT)

import Control.Applicative (empty,liftA2)
import Data.Void

import Data.Bifunctor (first)
import Data.Text (Text, pack, cons)

import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import DSL.Types
import DSL.Name
import DSL.Pretty ()
import DSL.Primitive

type Parser = Parsec Void Text

--
-- * Parse Expressions
--

topLevel :: Parser a -> Parser a
topLevel p = between sc eof p

-- | Parse a Text value as an expression.
parseExprText :: Text -> Either String Expr
parseExprText = first errorBundlePretty . parse (topLevel expr) ""

-- | Parse a String value as an expression.
parseExprString :: String -> Either String Expr
parseExprString = parseExprText . pack

-- | Parse a Text value as an expression.
parseBExprText :: Text -> Either String BExpr
parseBExprText = first errorBundlePretty . parse (topLevel bexpr) ""

-- | Parse a String value as an expression.
parseBExprString :: String -> Either String BExpr
parseBExprString = parseBExprText . pack

-- | Parse a Text value as a Value.
parseValueText :: Text -> Either String Value
parseValueText = first errorBundlePretty . parse (topLevel value) ""

-- | Parse a String value as an expression.
parseValueString :: String -> Either String Value
parseValueString = parseValueText . pack

--
-- * Internal
--

-- ** Lexer

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

verbatim :: Text -> Parser Text
verbatim = L.symbol sc

rws :: [Text]
rws = ["true", "false", "abs", "signum", "ceiling", "floor", "round", "if", "then", "else"]

rword :: Text -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar)

name :: Parser Char -> Parser Char -> Parser Name
name start rest = lexeme (liftA2 cons start (pack <$> many rest))

var :: Parser Var
var = try (name start rest >>= check) <?> "variable name"
  where
    start = choice [char '$', char '_', letterChar]
    rest  = choice [char '$', char '_', alphaNumChar]
    check x = if x `elem` rws
                then fail $ "Keyword " <> show x <> " cannot be a variable name."
                else return x

parens :: Parser a -> Parser a
parens = between (verbatim "(") (verbatim ")")

-- ** Basic Parsers

op fixity f name = fixity (f <$ verbatim name)

bool :: Parser Bool
bool = tru
    <|> fls
    <?> "boolean literal"
  where
    tru = True <$ rword "true"
    fls = False <$ rword "false"

int :: Parser Int
int = fmap fromInteger (lexeme L.decimal) <?> "integer literal"

float :: Parser Double
float = lexeme L.float <?> "float literal"

maybe' :: Parser a -> Parser (Maybe a)
maybe' some = none <|> Just <$> some <?> "maybe"
  where
    none = Nothing <$ rword "none"

-- ** Integer Expression Parser

opTableIExpr :: [[Operator Parser IExpr]]
opTableIExpr =
  [
    [op Prefix (OpI Neg) "-"]
    , map inNN_N [Mul,Div,Mod]
    , map inNN_N [Add,Sub]
  ]
  where
    inNN_N o = op InfixL (opNN_N o) (pretty o)

iterm :: Parser IExpr
iterm = parens iexpr
    <|> ilit
    <|> iref
    <|> ifun
    <?> "integer term"
  where
    ilit = ILit <$> int
    iref = IRef <$> var
    ifun = OpI <$> n_n <*> iexpr

n_n :: Parser N_N
n_n = abs
    <|> signum
    <?> "unary number operator"
  where
    abs = Abs <$ rword "abs"
    signum = Sign <$ rword "signum"

iexpr :: Parser IExpr
iexpr = makeExprParser iterm opTableIExpr <?> "integer expression"

-- ** Boolean Expression Parser

opTableBExpr :: [[Operator Parser BExpr]]
opTableBExpr =
  [
    [op Prefix (OpB Not) "!"]
    , [inBB_B And]
    , map inBB_B [Or,XOr]
    , [inBB_B Imp]
    , [inBB_B Eqv]
  ]
  where
    inBB_B o = op InfixR (opBB_B o) (pretty o)

relation :: Parser NN_B
relation = (verbatim (pretty LTE) *> pure LTE)
       <|> (verbatim (pretty LT) *> pure LT)
       <|> (verbatim (pretty Equ) *> pure Equ)
       <|> (verbatim (pretty Neq) *> pure Neq)
       <|> (verbatim (pretty GTE) *> pure GTE)
       <|> (verbatim (pretty GT) *> pure GT)
       <?> "relational operator"

bterm :: Parser BExpr
bterm = try (parens bexpr)
    <|> blit
    <|> try rexpr
    <|> bref
    <?> "boolean term"
  where
    blit = BLit <$> bool
    bref = BRef <$> var
    rexpr = do
      i1 <- iexpr
      op <- relation
      i2 <- iexpr
      return (OpIB op i1 i2)

bexpr :: Parser BExpr
bexpr = makeExprParser bterm opTableBExpr <?> "boolean expression"

-- ** Variational Parser

v :: Parser a -> Parser (V a)
v a = chc <|> one <?> "choice expression"
  where
    chc = do
      verbatim "["
      d <- bexpr
      verbatim "]"
      verbatim "{"
      l <- v a
      verbatim ","
      r <- v a
      verbatim "}"
      return (Chc d l r)
    one = One <$> a

v' :: Parser Expr -> Parser (V Expr)
v' e = chc <|> one <?> "choice expression"
  where
    chc = do
      verbatim "["
      d <- bexpr
      verbatim "]"
      verbatim "{"
      trylit d <|> ex d
    one = One <$> e
    trylit d = try (do
      l <- v pval
      verbatim ","
      r <- v pval
      verbatim "}"
      return (One . Lit $ Chc d l r))
    ex d = do
      l <- v' e
      verbatim ","
      r <- v' e
      verbatim "}"
      return (Chc d l r)

-- ** Value Parser

value :: Parser Value
value = v (maybe' pval)

-- ** Expression Parser

symbol :: Parser Symbol
symbol = char ':' >> Symbol <$> name start rest <?> "symbol"
  where
    start = char '_' <|> letterChar
    rest  = choice [char '_', char '-', alphaNumChar]

pval :: Parser PVal
pval = unit
    <|> b
    <|> s
    <|> f
    <|> i
    <?> "primitive value"
  where
    unit = Unit <$ verbatim "()"
    b = B <$> bool
    i = I <$> int
    f = F <$> try float
    s = S <$> symbol

path :: Parser Path
path = char '@' >> absolute <|> relative <?> "resource path"
  where
    absolute = char '/' >> Path Absolute <$> steps
    relative = Path Relative <$> steps
    steps = sepBy step (char '/')
    step = verbatim ".." <|> verbatim "." <|> (pack <$> many (char '_' <|> alphaNumChar))

keyword :: (Pretty a) => a -> Parser ()
keyword = rword . pretty

eterm :: Parser Expr
eterm =
    try (unit)
    <|> parens expr
    <|> lit
    <|> res
    <|> op1
    <|> op3
    <|> ref
    <?> "expression term"
  where
    unit = (Lit . One $ Unit) <$ verbatim "()"
    lit = Lit <$> v pval
    res = Res <$> path
    ref = Ref <$> var
    p1 o = P1 o <$ keyword o <*> v' expr
    op1 = p1 U_U
      <|> p1 (N_N Abs)
      <|> p1 (N_N Sign)
      <|> p1 (F_I Ceil)
      <|> p1 (F_I Floor)
      <|> p1 (F_I Round)
      <|> P1 (B_B Not) <$ verbatim "!" <*> v' eterm
      <|> P1 (N_N Neg) <$ verbatim "-" <*> v' eterm
    op3 = do
      rword "if"
      c <- v' expr
      rword "then"
      t <- v' expr
      rword "else"
      e <- v' expr
      return (P3 Cond c t e)

p2 :: Op2 -> Parser (V Expr -> V Expr -> Expr)
p2 o = P2 o <$ verbatim (pretty o)

eop1 :: Parser (V Expr -> V Expr -> Expr)
eop1 =
      p2 (NN_N Mul)
  <|> p2 (NN_N Div)
  <|> p2 (NN_N Mod)

eop2 :: Parser (V Expr -> V Expr -> Expr)
eop2 =
      p2 (NN_N Add)
  <|> p2 (NN_N Sub)

eop3 :: Parser (V Expr -> V Expr -> Expr)
eop3 =
      try (p2 (NN_B LTE))
  <|> try (p2 (NN_B GTE))
  <|> p2 (NN_B LT)
  <|> p2 (NN_B GT)
  <|> p2 (NN_B Neq)
  <|> p2 (NN_B Equ)
  <|> p2 (SS_B SEqu)

eop4 :: Parser (V Expr -> V Expr -> Expr)
eop4 =
     p2 (BB_B And)

eop5 :: Parser (V Expr -> V Expr -> Expr)
eop5 =
      p2 (BB_B Or)
  <|> p2 (BB_B XOr)

eop6 :: Parser (V Expr -> V Expr -> Expr)
eop6 =
      p2 (BB_B Imp)

eop7 :: Parser (V Expr -> V Expr -> Expr)
eop7 = p2 (BB_B Eqv)

pInfixL :: Parser (V a -> V a -> a) -> Parser (V a) -> V a -> Parser a
pInfixL op p x = do
  f <- op
  y <- p
  let r = f x y
  pInfixL op p (One r) <|> return r

pInfixN :: Parser (V a -> V a -> a) -> Parser (V a) -> V a -> Parser a
pInfixN op p x = do
  f <- op
  y <- p
  return $ f x y

pInfixR :: Parser (V a -> V a -> a) -> Parser (V a) -> V a -> Parser a
pInfixR op p x = do
  f <- op
  y <- p >>= \r -> (One <$> pInfixR op p r) <|> return r
  return $ f x y

expr' :: (Parser (V a -> V a -> a) -> Parser (V a) -> V a -> Parser a) ->
         Parser a ->
         Parser (V a -> V a -> a) ->
         Parser a
expr' p e op = e >>= (\x -> p op (One <$> e) (One x) <|> pure x)

exprL :: Parser a ->
         Parser (V a -> V a -> a) ->
         Parser a
exprL = expr' pInfixL

expr1 :: Parser Expr
expr1 = try (v' eterm >>= pInfixL eop1 (v' eterm)) <|> eterm

expr2 :: Parser Expr
expr2 = exprL expr1 eop2

expr3 :: Parser Expr
expr3 = expr' pInfixN expr2 eop3

expr4 :: Parser Expr
expr4 = exprL expr3 eop4

expr5 :: Parser Expr
expr5 = exprL expr4 eop5

expr6 :: Parser Expr
expr6 = expr' pInfixR expr5 eop6

expr7 :: Parser Expr
expr7 = exprL expr6 eop7

expr :: Parser Expr
expr = expr7 <?> "expression"
