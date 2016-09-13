module DSL.Pretty where

import Prelude hiding (LT,GT)

import Data.List (intercalate)

import DSL.Effect
import DSL.Environment
import DSL.Expression
import DSL.Primitive


--
-- * Resources
--

pPath :: Path -> String
pPath = intercalate "."

pEffect :: Effect -> String
pEffect (Create e) = "create " ++ pExpr e
pEffect (Check f)  = "check " ++ pFun f
pEffect (Modify f) = "modify " ++ pFun f
pEffect Delete     = "delete"

pEffectErrorKind :: EffectErrorKind -> String
pEffectErrorKind CheckFailure          = "Resource check failure"
pEffectErrorKind CheckTypeError        = "Type error on resource check"
pEffectErrorKind NoSuchResource        = "No such resource"
pEffectErrorKind ResourceAlreadyExists = "Resource already exists"

pEffectError :: EffectError -> String
pEffectError err = unlines $
    [ pEffectErrorKind (errorKind err) ++ ":"
    , "  At path: " ++ pPath (errorPath err)
    , "  While executing: " ++ pEffect (errorEffect err) ]
    ++ maybe [] (\v -> ["  Resource value: " ++ pPVal v]) (errorValue err)


--
-- * Expressions
--

-- ** Primitives

pPType :: PType -> String
pPType TUnit = "unit"
pPType TBool = "bool"
pPType TInt  = "int"

pPVal :: PVal -> String
pPVal Unit      = "()"
pPVal (B True)  = "true"
pPVal (B False) = "false"
pPVal (I i)     = show i


-- ** Symbol Names

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


-- ** Expressions

pTerm :: Expr -> String
pTerm (Ref x) = x
pTerm (Lit v) = pPVal v
pTerm e       = pParens (pExpr e)

pExpr :: Expr -> String
pExpr (Ref x)           = x
pExpr (Lit v)           = pPVal v
pExpr (P1 (B_B Not) e)  = "!" ++ pTerm e
pExpr (P1 (I_I Neg) e)  = "-" ++ pTerm e
pExpr (P2 (II_I o) l r) = concat [pTerm l, pII_I o, pTerm r]
pExpr (P2 o l r)        = intercalate " " [pTerm l, pOp2 o, pTerm r]
pExpr e = error $ "Couldn't pretty print expression: " ++ show e

pParens :: String -> String
pParens s = "(" ++ s ++ ")"


-- ** Functions

pParam :: Param -> String
pParam (P x t) = x ++ ":" ++ pPType t

pFun :: Fun -> String
pFun (Fun p e) = "λ" ++ pParam p ++ "." ++ pExpr e
