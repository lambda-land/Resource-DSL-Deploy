module DSL.Parser
  ( parseExprText, parseExprString, parseBExprText, parseBExprString
  ) where

import Prelude hiding (LT,GT)

import Control.Applicative (empty,liftA2)
import Data.Void

import Data.Bifunctor (first)
import Data.Text (Text, pack, cons)

import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import DSL.Types
import DSL.Name
import DSL.Pretty
import DSL.Primitive

type Parser = Parsec Void Text

--
-- * Parse Expressions
--

-- | Parse a Text value as an expression.
parseExprText :: Text -> Either String Expr
parseExprText = first parseErrorPretty . parse (expr <* eof) ""

-- | Parse a String value as an expression.
parseExprString :: String -> Either String Expr
parseExprString = parseExprText . pack

-- | Parse a Text value as an expression.
parseBExprText :: Text -> Either String BExpr
parseBExprText = first parseErrorPretty . parse (bexpr <* eof) ""

-- | Parse a String value as an expression.
parseBExprString :: String -> Either String BExpr
parseBExprString = parseBExprText . pack

--
-- * Internal
--

-- ** Lexer

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

verbatim :: Text -> Parser Text
verbatim = L.symbol spaceConsumer

reserved :: Text -> Parser ()
reserved w = lexeme (string w >> notFollowedBy alphaNumChar)


-- ** Expression Parser

bool :: Parser Bool
bool = tru <|> fls <?> "boolean literal"
  where
    tru = True <$ reserved "true"
    fls = False <$ reserved "false"

int :: Parser Int
int = fmap fromInteger (lexeme L.decimal) <?> "integer literal"

float :: Parser Double
float = lexeme L.float <?> "floating point literal"

name :: Parser Char -> Parser Char -> Parser Name
name start rest = lexeme (liftA2 cons start (pack <$> many rest))

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
    step = verbatim "." <|> verbatim ".." <|> (pack <$> many (char '_' <|> alphaNumChar))

parens :: Parser a -> Parser a
parens = between (lexeme (char '(')) (lexeme (char ')'))

op fixity f name = fixity (f <$ verbatim name)

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
iterm = parens iexpr <|> ilit <|> iref <|> ifun <?> "integer term"
  where
    ilit = ILit <$> int
    iref = IRef <$> var
    ifun = (OpI Abs) <$ reserved "abs" <*> iexpr <|> (OpI Sign) <$ reserved "signum" <*> iexpr

iexpr :: Parser IExpr
iexpr = makeExprParser iterm opTableIExpr

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
relation = verbatim (pretty LT) *> pure LT
       <|> verbatim (pretty LTE) *> pure LTE
       <|> verbatim (pretty Equ) *> pure Equ
       <|> verbatim (pretty Neq) *> pure Neq
       <|> verbatim (pretty GTE) *> pure GTE
       <|> verbatim (pretty GT) *> pure GT

bterm :: Parser BExpr
bterm = parens bexpr <|> blit <|> bref <|> rexpr <?> "boolean term"
  where
    blit = BLit <$> bool
    bref = BRef <$> var
    rexpr = do
      i1 <- iexpr
      op <- relation
      i2 <- iexpr
      return (OpIB op i1 i2)

bexpr :: Parser BExpr
bexpr = makeExprParser bterm opTableBExpr

v :: Parser a -> Parser (V a)
v a = chc <|> one <?> "choice expression"
  where
    chc = do
      d <- bexpr
      verbatim "{"
      l <- v a
      verbatim ","
      r <- v a
      verbatim "}"
      return (Chc d l r)
    one = One <$> a

pval :: Parser PVal
pval = unit <|> b <|> i <|> f <|> s <?> "primitive value"
  where
    unit = Unit <$ verbatim "()"
    b = B <$> bool
    i = I <$> int
    f = F <$> float
    s = S <$> symbol

eterm :: Parser Expr
eterm = parens expr <|> ref <|> res <|> lit <?> "expression term"
  where
    ref = Ref <$> var
    res = Res <$> path
    lit = Lit <$> v pval

b_b :: Parser B_B
b_b = Not <$ verbatim "!"

n_n :: Parser N_N
n_n = abs <|> neg <|> sign
  where
    neg = Neg <$ verbatim "!"
    abs = Abs <$ reserved "abs"
    sign = Sign <$ reserved "signum"

f_i :: Parser F_I
f_i = ceil <|> floor <|> round
  where
    ceil = Ceil <$ reserved "ceiling"
    floor = Floor <$ reserved "floor"
    round = Round <$ reserved "round"

op1 :: Parser Op1
op1 = u_u <|> b_b' <|> n_n' <|> f_i' <?> "unary operation"
  where
    u_u = U_U <$ verbatim "unit"
    b_b' = B_B <$> b_b
    n_n' = N_N <$> n_n
    f_i' = F_I <$> f_i

bb_b :: Parser BB_B
bb_b = and <|> or <|> xor <|> imp <|> eqv
  where
    and = inBB_B And
    or = inBB_B Or
    xor = inBB_B XOr
    imp = inBB_B Imp
    eqv = inBB_B Eqv
    inBB_B o = o <$ verbatim (pretty o)

nn_n :: Parser NN_N
nn_n = add <|> sub <|> mul <|> div <|> mod
  where
    add = inNN_N Add
    sub = inNN_N Sub
    mul = inNN_N Mul
    div = inNN_N Div
    mod = inNN_N Mod
    inNN_N o = o <$ verbatim (pretty o)

nn_b :: Parser NN_B
nn_b = lt <|> lte <|> eq <|> neq <|> gte <|> gt
  where
    lt = inNN_B LT
    lte = inNN_B LTE
    eq = inNN_B Equ
    neq = inNN_B Neq
    gte = inNN_B GTE
    gt = inNN_B GT
    inNN_B o = o <$ verbatim (pretty o)

op2 :: Parser Op2
op2 = bb_b' <|> nn_n' <|> nn_b'
  where
    bb_b' = BB_B <$> bb_b
    nn_n' = NN_N <$> nn_n
    nn_b' = NN_B <$> nn_b

eop :: Parser Expr
eop = p1 <|> p2 <|> p3 <?> "expression operation"
  where
    p1 = P1 <$> op1 <*> v expr
    p2 = P2 <$> op2 <*> v expr <*> v expr
    p3 = do
      c <- v expr
      verbatim "?"
      t <- v expr
      verbatim ":"
      e <- v expr
      return (P3 Cond c t e)

expr :: Parser Expr
expr = eterm <|> eop
