module DSL.Parser where

import Prelude hiding (LT,GT)

import Control.Applicative (empty)
import Control.Monad (void)

import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L

import DSL.Expression
import DSL.Pretty
import DSL.Primitive


--
-- * Lexer
--

spaceConsumer :: Parser ()
spaceConsumer = L.space (void spaceChar) empty empty

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer


--
-- * Expression Parser
--

-- | Operator table.
opTable :: [[Operator Parser Expr]]
opTable =
    [ [op Prefix negate "-", op Prefix bnot "!"]
    , map inII_I [Mul,Div,Mod]
    , map inII_I [Add,Sub]
    , map inII_B [LT,LTE,Equ,Neq,GTE,GT]
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
