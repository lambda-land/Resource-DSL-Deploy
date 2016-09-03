module DSL.Parser where

import Control.Applicative (empty)
import Control.Monad (void)

import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L

import DSL.Expression
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
    [ [ prefix "-"   (P1 (I_I Neg))
      , prefix "!"   bnot  ]
    , [ infixL "*"   (*)
      , infixL "/"   (./)
      , infixL "%"   (.%)  ]
    , [ infixL "+"   (+)
      , infixL "-"   (-)   ]
    , [ infixN "=="  (.==)
      , infixN "!="  (./=)
      , infixN "<"   (.<)
      , infixN "<="  (.<=)
      , infixN ">="  (.>=)
      , infixN ">"   (.>)  ]
    , [ infixR "&"   (&&&) ]
    , [ infixR "|"   (|||)
      , infixR "><"  (<+>) ]
    , [ infixR "=>"  (==>) ]
    , [ infixR "<=>" (<=>) ] ]
  where
    prefix n f = Prefix (f <$ symbol n)
    infixN n f = InfixN (f <$ symbol n)
    infixL n f = InfixL (f <$ symbol n)
    infixR n f = InfixR (f <$ symbol n)
