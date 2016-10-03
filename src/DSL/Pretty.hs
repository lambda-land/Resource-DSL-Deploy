module DSL.Pretty where

import Prelude hiding (LT,GT)

import Data.List (intercalate)

import DSL.Effect
import DSL.Environment
import DSL.Expression
import DSL.Name
import DSL.Path
import DSL.Primitive


--
-- * Resources
--

prettyPath :: Path -> String
prettyPath (Path k p) = case k of
    Absolute -> '/' : path
    Relative -> path
  where path = intercalate "/" p

prettyResID :: ResID -> String
prettyResID (ResID p) = intercalate "/" p

prettyEffect :: Effect -> String
prettyEffect (Create e) = "create " ++ prettyExpr e
prettyEffect (Check f)  = "check "  ++ prettyFun f
prettyEffect (Modify f) = "modify " ++ prettyFun f
prettyEffect Delete     = "delete"

prettyEffectErrorKind :: EffectErrorKind -> String
prettyEffectErrorKind CheckFailure          = "Resource check failure"
prettyEffectErrorKind CheckTypeError        = "Type error on resource check"
prettyEffectErrorKind NoSuchResource        = "No such resource"
prettyEffectErrorKind ResourceAlreadyExists = "Resource already exists"

prettyEffectError :: EffectError -> String
prettyEffectError err = unlines $
    [ prettyEffectErrorKind (errorKind err) ++ ":"
    , "  On resource: " ++ prettyResID (errorResID err)
    , "  While executing: " ++ prettyEffect (errorEffect err) ]
    ++ maybe [] (\v -> ["  Resource value: " ++ prettyPVal v]) (errorValue err)


--
-- * Expressions
--

-- ** Primitives

prettyPType :: PType -> String
prettyPType TUnit   = "unit"
prettyPType TBool   = "bool"
prettyPType TInt    = "int"
prettyPType TSymbol = "symbol"

prettyPVal :: PVal -> String
prettyPVal Unit      = "()"
prettyPVal (B True)  = "true"
prettyPVal (B False) = "false"
prettyPVal (I i)     = show i
prettyPVal (S s)     = toName s


-- ** Operator Names

prettyOp2 :: Op2 -> String
prettyOp2 (BB_B o) = prettyBB_B o
prettyOp2 (II_B o) = prettyII_B o
prettyOp2 (II_I o) = prettyII_I o

prettyBB_B :: BB_B -> String
prettyBB_B And = "&&"
prettyBB_B Or  = "||"
prettyBB_B XOr = "><"
prettyBB_B Imp = "->"
prettyBB_B Eqv = "<->"

prettyII_B :: II_B -> String
prettyII_B LT  = "<"
prettyII_B LTE = "<="
prettyII_B Equ = "=="
prettyII_B Neq = "!="
prettyII_B GTE = ">="
prettyII_B GT  = ">"

prettyII_I :: II_I -> String
prettyII_I Add = "+"
prettyII_I Sub = "-"
prettyII_I Mul = "*"
prettyII_I Div = "/"
prettyII_I Mod = "%"


-- ** Expressions

prettyTerm :: Expr -> String
prettyTerm (Ref x) = x
prettyTerm (Lit v) = prettyPVal v
prettyTerm e       = prettyParens (prettyExpr e)

prettyExpr :: Expr -> String
prettyExpr (Ref x)           = x
prettyExpr (Lit v)           = prettyPVal v
prettyExpr (P1 (B_B Not) e)  = "!" ++ prettyTerm e
prettyExpr (P1 (I_I Neg) e)  = "-" ++ prettyTerm e
prettyExpr (P2 (II_I o) l r) = concat [prettyTerm l, prettyII_I o, prettyTerm r]
prettyExpr (P2 o l r)        = intercalate " " [prettyTerm l, prettyOp2 o, prettyTerm r]
prettyExpr e = error $ "Couldn't pretty print expression: " ++ show e

prettyParens :: String -> String
prettyParens s = "(" ++ s ++ ")"


-- ** Functions

prettyParam :: Param -> String
prettyParam (Param x t) = x ++ ":" ++ prettyPType t

prettyFun :: Fun -> String
prettyFun (Fun p e) = "λ" ++ prettyParam p ++ "." ++ prettyExpr e
