module DSL.Pretty where

import Prelude hiding (LT,GT)

import Data.List (intercalate)

import DSL.Effect
import DSL.Expression
import DSL.Model
import DSL.Name
import DSL.Path
import DSL.Primitive


--
-- * Resources
--

-- ** Paths and Resource IDs

prettyPath :: Path -> String
prettyPath (Path k p) = case k of
    Absolute -> '/' : path
    Relative -> path
  where path = intercalate "/" p

prettyResID :: ResID -> String
prettyResID (ResID p) = intercalate "/" p


-- ** Effects

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
prettyEffectError (EffectError eff kind rID mval) = unlines $
    [ prettyEffectErrorKind kind ++ ":"
    , "  On resource: " ++ prettyResID rID
    , "  While executing: " ++ prettyEffect eff ]
    ++ maybe [] (\v -> ["  Resource value: " ++ prettyPVal v]) mval


-- ** Models

prettyStmtErrorKind :: StmtErrorKind -> String
prettyStmtErrorKind IfTypeError   = "Non-Boolean condition"
prettyStmtErrorKind ForTypeError  = "Non-integer range bound"
prettyStmtErrorKind LoadTypeError = "Not a component ID"

prettyStmtError :: StmtError -> String
prettyStmtError (StmtError stmt kind val) = unlines
    [ prettyStmtErrorKind kind ++ ":"
    , "  In statement: " ++ show stmt  -- TODO: pretty print statements
    , "  Offending value: " ++ prettyPVal val ]


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
prettyExpr (Res p)           = "@" ++ prettyPath p
prettyExpr (Lit v)           = prettyPVal v
prettyExpr (P1 (B_B Not) e)  = "!" ++ prettyTerm e
prettyExpr (P1 (I_I Neg) e)  = "-" ++ prettyTerm e
prettyExpr (P2 (II_I o) l r) = concat [prettyTerm l, prettyII_I o, prettyTerm r]
prettyExpr (P2 o l r)        = unwords [prettyTerm l, prettyOp2 o, prettyTerm r]
prettyExpr (P3 Cond c l r)   = unwords [prettyTerm c, "?", prettyTerm l, ":", prettyTerm r]
prettyExpr e = error $ "Couldn't pretty print expression: " ++ show e

prettyParens :: String -> String
prettyParens s = "(" ++ s ++ ")"


-- ** Functions

prettyParam :: Param -> String
prettyParam (Param x t) = x ++ ":" ++ prettyPType t

prettyFun :: Fun -> String
prettyFun (Fun p e) = "λ" ++ prettyParam p ++ "." ++ prettyExpr e
