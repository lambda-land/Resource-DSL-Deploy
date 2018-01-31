module DSL.Pretty where

import Prelude hiding (LT,GT)

import Data.List (intercalate)

import DSL.Types
import DSL.Name

class Pretty a where
  pretty :: a -> String

instance (Pretty a) => Pretty (V a) where
  pretty (One a) = pretty a
  pretty (Chc d l r) = show d ++ "<" ++ pretty l ++ "," ++ pretty r ++ ">"

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
prettyEffect (Create e) = "create " ++ pretty e
prettyEffect (Check f)  = "check "  ++ prettyFun f
prettyEffect (Modify f) = "modify " ++ prettyFun f
prettyEffect Delete     = "delete"

instance Pretty Effect where
  pretty = prettyEffect

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
    ++ maybe [] (\v -> ["  Resource value: " ++ pretty v]) mval

instance (Pretty a) => Pretty (Maybe a) where
  pretty Nothing = "Nothing"
  pretty (Just a) = "Just (" ++ pretty a ++ ")"


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
prettyPType TFloat  = "float"
prettyPType TSymbol = "symbol"

instance Pretty PType where
  pretty = prettyPType

prettyPVal :: PVal -> String
prettyPVal Unit      = "()"
prettyPVal (B True)  = "true"
prettyPVal (B False) = "false"
prettyPVal (I i)     = show i
prettyPVal (F f)     = show f
prettyPVal (S s)     = toName s

instance Pretty PVal where
  pretty = prettyPVal


-- ** Operator Names

prettyOp2 :: Op2 -> String
prettyOp2 (BB_B o) = prettyBB_B o
prettyOp2 (NN_B o) = prettyNN_B o
prettyOp2 (NN_N o) = prettyNN_N o

prettyBB_B :: BB_B -> String
prettyBB_B And = "&&"
prettyBB_B Or  = "||"
prettyBB_B XOr = "><"
prettyBB_B Imp = "->"
prettyBB_B Eqv = "<->"

prettyNN_B :: NN_B -> String
prettyNN_B LT  = "<"
prettyNN_B LTE = "<="
prettyNN_B Equ = "=="
prettyNN_B Neq = "!="
prettyNN_B GTE = ">="
prettyNN_B GT  = ">"

prettyNN_N :: NN_N -> String
prettyNN_N Add = "+"
prettyNN_N Sub = "-"
prettyNN_N Mul = "*"
prettyNN_N Div = "/"
prettyNN_N Mod = "%"


-- ** Expressions
prettyTerm :: V Expr -> String
prettyTerm (One (Ref x)) = x
prettyTerm (One (Lit v)) = pretty v
prettyTerm (One e)       = prettyParens (prettyExpr e)
prettyTerm ve = pretty ve

prettyExpr :: Expr -> String
prettyExpr (Ref x)           = x
prettyExpr (Res p)           = "@" ++ prettyPath p
prettyExpr (Lit v)           = pretty v
prettyExpr (P1 o e)          = prettyP1 o e
prettyExpr (P2 (NN_N o) l r) = concat [prettyTerm l, prettyNN_N o, prettyTerm r]
prettyExpr (P2 o l r)        = unwords [prettyTerm l, prettyOp2 o, prettyTerm r]
prettyExpr (P3 Cond c l r)   = unwords [prettyTerm c, "?", prettyTerm l, ":", prettyTerm r]

instance Pretty Expr where
  pretty = prettyExpr

prettyP1 :: Op1 -> V Expr -> String
prettyP1 U_U         e = prettyPrimFun "unit" e
prettyP1 (B_B Not)   e = "!" ++ pretty e
prettyP1 (N_N Abs)   e = prettyPrimFun "abs" e
prettyP1 (N_N Neg)   e = "-" ++ pretty e
prettyP1 (N_N Sign)  e = prettyPrimFun "signum" e
prettyP1 (F_I Ceil)  e = prettyPrimFun "ceiling" e
prettyP1 (F_I Floor) e = prettyPrimFun "floor" e
prettyP1 (F_I Round) e = prettyPrimFun "round" e

prettyParens :: String -> String
prettyParens s = "(" ++ s ++ ")"

prettyPrimFun :: String -> V Expr -> String
prettyPrimFun n e = n ++ prettyParens (pretty e)


-- ** Functions

prettyParam :: Param -> String
prettyParam (Param x t) = x ++ ":" ++ pretty t

prettyFun :: Fun -> String
prettyFun (Fun p e) = "λ" ++ prettyParam p ++ ". " ++ pretty e
