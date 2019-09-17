module DSL.Pretty where

import Prelude hiding (LT,GT,concat,unwords,unlines)
import Data.Text

import DSL.Types


-- | Class of things that can be pretty printed.
class Pretty a where
  pretty :: a -> Text

-- | Pretty print a term within a larger expression.
class Pretty a => PrettyTerm a where
  prettyTerm :: a -> Text
  prettyTerm = pretty

prettyParens :: Text -> Text
prettyParens t = concat ["(", t, ")"]

prettyString :: Pretty a => a -> String
prettyString = unpack . pretty


-- ** Paths

instance Pretty Path where
  pretty (Path k ns) = case k of
      Absolute -> '/' `cons` p
      Relative -> p
    where p = intercalate "/" ns

instance Pretty ResID where
  pretty (ResID ns) = intercalate "/" ns


-- ** Primitives

instance Pretty Bool where
  pretty True  = "true"
  pretty False = "false"

instance Pretty Int where
  pretty = pack . show

instance Pretty Double where
  pretty = pack . show

instance Pretty a => Pretty (Maybe a) where
  pretty Nothing  = "_"
  pretty (Just a) = pretty a

instance Pretty PType where
  pretty TUnit   = "unit"
  pretty TBool   = "bool"
  pretty TInt    = "int"
  pretty TFloat  = "float"
  pretty TString = "string"

instance Pretty PVal where
  pretty Unit      = "()"
  pretty (B True)  = "true"
  pretty (B False) = "false"
  pretty (I i)     = pretty i
  pretty (F f)     = pretty f
  pretty (S t)     = concat ["\"", t, "\""]


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

instance Pretty Op1 where
  pretty U_U         = "unit"
  pretty (B_B Not)   = "!"
  pretty (N_N Abs)   = "abs"
  pretty (N_N Neg)   = "-"
  pretty (N_N Sign)  = "signum"
  pretty (F_I Ceil)  = "ceiling"
  pretty (F_I Floor) = "floor"
  pretty (F_I Round) = "round"

instance Pretty Op2 where
  pretty (BB_B o) = pretty o
  pretty (NN_B o) = pretty o
  pretty (NN_N o) = pretty o
  pretty (SS_B o) = pretty o


-- ** Predicates

instance PrettyTerm BExpr where
  prettyTerm (BLit b) = pretty b
  prettyTerm (BRef v) = v
  prettyTerm e = prettyParens (pretty e)

instance Pretty BExpr where
  pretty (OpB Not e)  = '!' `cons` prettyTerm e
  pretty (OpBB o l r) = concat [prettyTerm l, pretty o, prettyTerm r]
  pretty (OpIB o l r) = concat [prettyTerm l, pretty o, prettyTerm r]
  pretty e = prettyTerm e

instance PrettyTerm IExpr where
  prettyTerm (ILit i)
    | i < 0     = prettyParens (pretty i)
    | otherwise = pretty i
  prettyTerm (IRef var) = var
  prettyTerm ie = prettyParens (pretty ie)

instance Pretty IExpr where
  pretty (OpI Abs e)  = "abs " `append` prettyTerm e
  pretty (OpI Neg e)  = '-' `cons` prettyTerm e
  pretty (OpI Sign e) = "signum " `append` prettyTerm e
  pretty (OpII o l r) = concat [prettyTerm l, pretty o, prettyTerm r]
  pretty e = prettyTerm e


-- ** Variation

instance PrettyTerm a => PrettyTerm (V a) where
  prettyTerm (One a) = prettyTerm a
  prettyTerm v = pretty v

instance Pretty a => Pretty (V a) where
  pretty (One a) = pretty a
  pretty (Chc d l r) = concat ["[", prettyTerm d, "]{", pretty l, ",", pretty r, "}"]


-- ** Functions and expressions

instance Pretty Param where
  pretty (Param x t) = concat [x, ":", pretty t]

instance Pretty Fun where
  pretty (Fun p e) = concat ["λ", pretty p, ". ", pretty e]

instance PrettyTerm Expr where
  prettyTerm (Ref x) = x
  prettyTerm (Lit v) = pretty v
  prettyTerm e       = prettyParens (pretty e)

instance Pretty Expr where
  pretty (Ref x)           = x
  pretty (Res p)           = '@' `cons` pretty p
  pretty (Lit v)           = pretty v
  pretty (P1 (B_B Not) e)  = '!' `cons` prettyTerm e
  pretty (P1 (N_N Neg) e)  = '-' `cons` prettyTerm e
  pretty (P1 o e)          = pretty o `append` prettyTerm e
  pretty (P2 (NN_N o) l r) = concat [prettyTerm l, pretty o, prettyTerm r]
  pretty (P2 o l r)        = unwords [prettyTerm l, pretty o, prettyTerm r]
  pretty (P3 Cond c l r)   = unwords ["if", prettyTerm c, "then", prettyTerm l, "else", prettyTerm r]


-- ** Effects

instance Pretty Effect where
  pretty (Create e) = "create " `append` pretty e
  pretty (Check f)  = "check "  `append` pretty f
  pretty (Modify f) = "modify " `append` pretty f
  pretty Delete     = "delete"


-- ** Errors

instance Pretty Error where
  
  pretty (ResNotFound r) = "Resource not found: " <> pretty r
  
  pretty (VarNotFound x) = "Variable not found: " <> x

  pretty (CompNotFound c) = "Component not found in dictionary: " <> c
  
  pretty (CannotNormalize p) = "Cannot normalize path: " <> pretty p
  
  pretty (ArgTypeError p v ) = unlines
    [ "Argument type error: "
    , "  Parameter declaration: " <> pretty p
    , "  Passed argument: " <> pretty v ]
  
  pretty (PrimTypeError1 o p) =
      "Primitive type error: "
      <> "attempting to apply " <> pretty o
      <> " to the value " <> pretty p

  pretty (PrimTypeError2 o p1 p2) =
      "Primitive type error: "
      <> "attempting to apply " <> pretty o
      <> " to the values " <> pretty p1 <> " and " <> pretty p2

  pretty (PrimTypeError3 _ p1 p2 p3) =
      "Primitive type error: "
      <> "in the conditional expression: "
      <> unwords ["if", pretty p1, "then", pretty p2, "else", pretty p3]

  pretty (EffectError k eff rID v) = unlines
      [ pretty k `snoc` ':'
      , "  On resource: " `append` pretty rID
      , "  While executing: " `append` pretty eff
      , "  Resource value: " `append` pretty v ]
  
  pretty (StmtError k s v) = unlines
      [ pretty k `snoc` ':'
      , "  In statement: " `append` pack (show s)  -- TODO: pretty print statements
      , "  Offending value: " `append` pretty v ]
  
instance Pretty EffectErrorKind where
  pretty CheckFailure        = "Resource check failure"
  pretty CheckTypeError      = "Type error on resource check"
  pretty CreateAlreadyExists = "Resource already exists"

instance Pretty StmtErrorKind where
  pretty IfTypeError   = "Non-Boolean condition"
  pretty LoadTypeError = "Not a component ID"
