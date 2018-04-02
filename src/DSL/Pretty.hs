module DSL.Pretty where

import Prelude hiding (LT,GT,concat,unwords,unlines)
import Data.Text
import Data.Monoid


import DSL.Types
import DSL.Name

class (Pretty a) => PrettyTerm a where
  prettyTerm :: a -> Text
  prettyTerm = pretty

instance Pretty Bool where
  pretty True = "true"
  pretty False = "false"

instance Pretty Int where
  pretty = pack . show

instance Pretty Double where
  pretty = pack . show

instance (Pretty a) => Pretty (Maybe a) where
  pretty Nothing = "Nothing"
  pretty (Just a) = concat ["Just (", pretty a, ")"]

prettyParens :: Text -> Text
prettyParens t = concat ["(", t, ")"]

-- ** Operator Names

instance Pretty BB_B where
  pretty And = "&&"
  pretty Or  = "||"
  pretty XOr = "><"
  pretty Imp = "->"
  pretty Eqv = "<->"

instance Pretty NN_B where
  pretty LT  = "<"
  pretty LTE = "<="
  pretty Equ = "=="
  pretty Neq = "!="
  pretty GTE = ">="
  pretty GT  = ">"

instance Pretty NN_N where
  pretty Add = "+"
  pretty Sub = "-"
  pretty Mul = "*"
  pretty Div = "/"
  pretty Mod = "%"

instance Pretty SS_B where
  pretty SEqu = "~"

instance Pretty Op2 where
  pretty (BB_B o) = pretty o
  pretty (NN_B o) = pretty o
  pretty (NN_N o) = pretty o
  pretty (SS_B o) = pretty o

-- ** Integer Expressions

instance PrettyTerm IExpr where
  prettyTerm (ILit i) | i < 0 = prettyParens (pretty i)
                      | otherwise = pretty i
  prettyTerm (IRef var) = var
  prettyTerm ie = prettyParens (pretty ie)

instance Pretty IExpr where
  pretty (OpI Abs iexpr) = "abs " `append` prettyTerm iexpr
  pretty (OpI Neg iexpr) = '-' `cons` prettyTerm iexpr
  pretty (OpI Sign iexpr) = "signum " `append` prettyTerm iexpr
  pretty (OpII o l r) = concat [prettyTerm l, pretty o, prettyTerm r]
  pretty ie = prettyTerm ie

-- ** Boolean Expressions

instance PrettyTerm BExpr where
  prettyTerm (BLit b) = pretty b
  prettyTerm (BRef var) = var
  prettyTerm be = prettyParens (pretty be)

instance Pretty BExpr where
  pretty (OpB Not bexpr) = '!' `cons` prettyTerm bexpr
  pretty (OpBB o l r) = concat [prettyTerm l, pretty o, prettyTerm r]
  pretty (OpIB o l r) = concat [prettyTerm l, pretty o, prettyTerm r]
  pretty be = prettyTerm be

-- ** Variability

instance (PrettyTerm a) => PrettyTerm (V a) where
  prettyTerm (One a) = prettyTerm a
  prettyTerm v = pretty v

instance (Pretty a) => Pretty (V a) where
  pretty (One a) = pretty a
  pretty (Chc d l r) = concat ["[", prettyTerm d, "]{", pretty l, ",", pretty r, "}"]

--
-- * Resources
--

-- ** Paths and Resource IDs

prettyPath :: Path -> Text
prettyPath (Path k p) = case k of
    Absolute -> '/' `cons` path
    Relative -> path
  where path = intercalate "/" p

instance Pretty ResID where
  pretty (ResID p) = intercalate "/" p

--
-- * Expressions
--

-- ** Primitives

instance Pretty PType where
  pretty TUnit   = "unit"
  pretty TBool   = "bool"
  pretty TInt    = "int"
  pretty TFloat  = "float"
  pretty TSymbol = "symbol"

instance Pretty PVal where
  pretty Unit      = "()"
  pretty (B True)  = "true"
  pretty (B False) = "false"
  pretty (I i)     = pretty i
  pretty (F f)     = pretty f
  pretty (S s)     = toName s

-- ** Expressions

instance PrettyTerm Expr where
  prettyTerm (Ref x) = x
  prettyTerm (Lit v) = pretty v
  prettyTerm e       = prettyParens (pretty e)

instance Pretty Expr where
  pretty (Ref x)           = x
  pretty (Res p)           = '@' `cons` prettyPath p
  pretty (Lit v)           = pretty v
  pretty (P1 o e)          = prettyP1 o e
  pretty (P2 (NN_N o) l r) = concat [prettyTerm l, pretty o, prettyTerm r]
  pretty (P2 o l r)        = unwords [prettyTerm l, pretty o, prettyTerm r]
  pretty (P3 Cond c l r)   = unwords ["if", prettyTerm c, "then", prettyTerm l, "else", prettyTerm r]

prettyP1 :: Op1 -> V Expr -> Text
prettyP1 U_U         e = prettyPrimFun "unit" e
prettyP1 (B_B Not)   e = '!' `cons` prettyTerm e
prettyP1 (N_N Abs)   e = prettyPrimFun "abs" e
prettyP1 (N_N Neg)   e = '-' `cons` prettyTerm e
prettyP1 (N_N Sign)  e = prettyPrimFun "signum" e
prettyP1 (F_I Ceil)  e = prettyPrimFun "ceiling" e
prettyP1 (F_I Floor) e = prettyPrimFun "floor" e
prettyP1 (F_I Round) e = prettyPrimFun "round" e


prettyPrimFun :: Text -> V Expr -> Text
prettyPrimFun n e = n `append` prettyTerm e

-- ** Functions

instance Pretty Param where
  pretty (Param x t) = concat [x, ":", pretty t]

instance Pretty Fun where
  pretty (Fun p e) = concat ["λ", pretty p, ". ", pretty e]

-- ** Effects

instance Pretty Effect where
  pretty (Create e) = "create " `append` pretty e
  pretty (Check f)  = "check "  `append` pretty f
  pretty (Modify f) = "modify " `append` pretty f
  pretty Delete     = "delete"

instance Pretty EffectErrorKind where
  pretty CheckFailure          = "Resource check failure"
  pretty CheckTypeError        = "Type error on resource check"
  pretty NoSuchResource        = "No such resource"
  pretty ResourceAlreadyExists = "Resource already exists"

instance Pretty EffectError where
  pretty (EffectError eff kind rID mval) = unlines $
    [ pretty kind `snoc` ':'
    , "  On resource: " `append` pretty rID
    , "  While executing: " `append` pretty eff ]
    ++ maybe [] (\v -> ["  Resource value: " `append` pretty v]) mval

-- ** Models

instance Pretty StmtErrorKind where
  pretty IfTypeError   = "Non-Boolean condition"
  pretty ForTypeError  = "Non-integer range bound"
  pretty LoadTypeError = "Not a component ID"

instance Pretty StmtError where
  pretty (StmtError stmt kind val) = unlines
      [ pretty kind `snoc` ':'
      , "  In statement: " `append` (pack . show) stmt  -- TODO: pretty print statements
      , "  Offending value: " `append` pretty val ]

instance Pretty Error where
  pretty (EnvE e) = pretty e
  pretty (PathE e) = pretty e
  pretty (PrimE e) = pretty e
  pretty (ExprE e) = pretty e
  pretty (EffE e) = pretty e
  pretty (StmtE e) = pretty e

instance Pretty NotFound where
  pretty (NotFound k ks) = "Key \"" <> (pack . show) k <> "\" not found in environment " <> (pack . show) ks

instance Pretty PathError where
  pretty (CannotNormalize p) = "Cannot normalize path " <> prettyPath p

instance Pretty Op1 where
  pretty U_U = "unit"
  pretty (B_B Not) = "!"
  pretty (N_N Abs) = "abs"
  pretty (N_N Neg) = "-"
  pretty (N_N Sign) = "signum"
  pretty (F_I Ceil) = "ceiling"
  pretty (F_I Floor) = "floor"
  pretty (F_I Round) = "round"

instance Pretty PrimTypeError where
  pretty (ErrorOp1 o p) = "Primitive type error: " <>
    "attempting to apply " <> pretty o <> " to the value " <> pretty p
  pretty (ErrorOp2 o p1 p2) = "Primitive type error: " <>
    "attempting to apply " <> pretty o <> " to the values " <> pretty p1 <> " and " <> pretty p2
  pretty (ErrorOp3 _ p1 p2 p3) = "Primitive type error: " <>
    "in the conditional expression if " <> pretty p1 <> " then " <> pretty p2 <> " else " <> pretty p3

instance Pretty ExprError where
  pretty (ArgTypeError param v pt pv) = "Argument type error: " <>
    "Expected a value of type " <> pretty pt <> " but got value \"" <> pretty pv <>
    "\" in parameter " <> pretty param <> " applied to the value " <> pretty v
  pretty (VarNotFound (NF (NotFound k ks))) = "Variable \"" <> (pack . show) k <> "\" not found in environment " <> (pack . show) ks
  pretty (VarNotFound (VNF k d v)) = "Variable \"" <> (pack . show) k <> "\" not found in variational value " <>
    pretty v <> " in context " <> pretty d
  pretty (ResNotFound (NF (NotFound k ks))) = "Resource \"" <> (pack . show) k <> "\" not found in environment " <> (pack . show) ks
  pretty (ResNotFound (VNF k d v)) = "Resource \"" <> (pack . show) k <> "\" not found in variational value " <>
    pretty v <> " in context " <> pretty d
