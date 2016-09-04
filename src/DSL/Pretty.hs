module DSL.Pretty where

import Prelude hiding (LT,GT)

import DSL.Primitive

--
-- * Keyword and Symbol Names
--

pOp2 :: Op2 -> String
pOp2 (BB_B o) = pBB_B o
pOp2 (II_B o) = pII_B o
pOp2 (II_I o) = pII_I o

pBB_B :: BB_B -> String
pBB_B And = "&&"
pBB_B Or  = "||"
pBB_B XOr = "><"
pBB_B Imp = "->"
pBB_B Eqv = "<->"

pII_B :: II_B -> String
pII_B LT  = "<"
pII_B LTE = "<="
pII_B Equ = "=="
pII_B Neq = "!="
pII_B GTE = ">="
pII_B GT  = ">"

pII_I :: II_I -> String
pII_I Add = "+"
pII_I Sub = "-"
pII_I Mul = "*"
pII_I Div = "/"
pII_I Mod = "%"
