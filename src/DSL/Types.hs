{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}

module DSL.Types where

import Data.Data (Typeable)
import GHC.Generics (Generic)

import Prelude hiding (LT,GT)

import Data.Fixed (mod')
import Data.Map.Strict (Map)
import Data.String (IsString(..))
import Data.Text (Text,pack,splitOn)
import Z3.Monad (AST)

import DSL.Boolean


--
-- * Names and Paths
--

-- | Miscellaneous name.
type Name = Text

-- | Variable name.
type Var = Name

-- | A path is either absolute or relative.
data PathKind = Absolute | Relative
  deriving (Eq,Generic,Ord,Read,Show,Typeable)

-- | A path through a resource environment. Unlike resource IDs, paths may be
--   relative, may contain "special" path names (such as ".." to refer to the
--   parent node), and may be appended with other paths. The idea is that a
--   resource ID is a simple key in the resource environment, while a path is
--   an intermediate object that can be manipulated in the language.
data Path = Path PathKind [Name]
  deriving (Eq,Generic,Ord,Read,Show,Typeable)

-- | Resource IDs are absolute paths from the root of the resource environment.
newtype ResID = ResID [Name]
  deriving (Eq,Generic,Monoid,Ord,Read,Semigroup,Show,Typeable)

instance IsString Path where
  fromString ('/':s) = Path Absolute (splitOn "/" (pack s))
  fromString s       = Path Relative (splitOn "/" (pack s))

instance IsString ResID where
  fromString ('/':s) = ResID (splitOn "/" (pack s))
  fromString s       = ResID (splitOn "/" (pack s))


--
-- * Environments
--

-- | An environment is a map from keys to values.
newtype Env k v = Env { envAsMap :: Map k v }
  deriving (Eq,Foldable,Functor,Generic,Ord,Read,Show,Typeable)

-- | Error thrown when a name is not found in the environment.
data NotFound k = NotFound k
  deriving (Eq,Generic,Ord,Read,Show,Typeable)


--
-- * Primitives
--

-- | Primitive base types.
data PType
   = TUnit
   | TBool
   | TInt
   | TFloat
   | TString
  deriving (Eq,Generic,Ord,Read,Show,Typeable)

-- | Primitive values.
data PVal
   = Unit
   | B Bool
   | I Int
   | F Double
   | S Text
  deriving (Eq,Generic,Ord,Read,Show,Typeable)

instance IsString PVal where
  fromString = S . fromString

-- | Primitive unary operators organized by type.
data Op1
   = U_U        -- ^ noop that matches a unit value
   | B_B B_B    -- ^ unary boolean operator
   | N_N N_N    -- ^ unary numeric operator
   | F_I F_I    -- ^ unary float-to-integer operator
  deriving (Eq,Generic,Ord,Read,Show,Typeable)

-- | Primitive binary operators organized by type.
data Op2
   = BB_B BB_B  -- ^ binary boolean operator
   | NN_N NN_N  -- ^ binary numeric operator
   | NN_B NN_B  -- ^ numeric comparison operator
   | SS_B SS_B  -- ^ string comparison operator
  deriving (Eq,Generic,Ord,Read,Show,Typeable)

-- | Primitive ternary operator.
data Op3 = OpIf
  deriving (Eq,Generic,Ord,Read,Show,Typeable)

-- | Boolean negation.
data B_B = Not
  deriving (Eq,Generic,Ord,Read,Show,Typeable)

-- | Unary numeric operators.
data N_N = Abs | Neg | Sign
  deriving (Eq,Generic,Ord,Read,Show,Typeable)

-- | Unary float-to-integer operators.
data F_I = Ceil | Floor | Round
  deriving (Eq,Generic,Ord,Read,Show,Typeable)

-- | Binary boolean operators.
data BB_B = And | Or | XOr | Imp | Eqv
  deriving (Eq,Generic,Ord,Read,Show,Typeable)

-- | Binary numeric comparison operators.
data NN_B = LT | LTE | Equ | Neq | GTE | GT
  deriving (Eq,Generic,Ord,Read,Show,Typeable)

-- | Binary numeric arithmetic operators.
data NN_N = Add | Sub | Mul | Div | Mod
  deriving (Eq,Generic,Ord,Read,Show,Typeable)

data SS_B = SEqu
  deriving (Eq,Generic,Ord,Read,Show,Typeable)

-- | Add division and modulus to the Num type class.
class Num n => PrimN n where
  (./), (.%) :: n -> n -> n

-- | A type class for overloading the primitive operators.
class (Boolean b, PrimN n) => Prim b n where
  (.<), (.<=), (.==), (./=), (.>=), (.>) :: n -> n -> b

infix 4 .<, .<=, .==, ./=, .>=, .>
infixl 7 ./, .%


-- Ground instances

instance PrimN Int where
  (./) = div
  (.%) = mod

instance PrimN Double where
  (./) = (/)
  (.%) = mod'

instance Prim Bool Int where
  (.<)  = (<)
  (.<=) = (<=)
  (.==) = (==)
  (./=) = (/=)
  (.>=) = (>=)
  (.>)  = (>)

instance Prim Bool Double where
  (.<)  = (<)
  (.<=) = (<=)
  (.==) = (==)
  (./=) = (/=)
  (.>=) = (>=)
  (.>)  = (>)


--
-- * Conditions
--

-- | A condition on a choice. A condition is a boolean expression over
--   configuration options. Additionally, this data type contains an
--   optionally cached symbolic encoding of a condition for use during
--   SAT solving.
data Cond = Cond {
  condExpr :: BExpr,     -- ^ boolean expression
  condSym  :: Maybe AST  -- ^ cached symbolic encoding
} deriving (Eq,Generic,Ord,Show)

-- | Boolean expressions with variable references.
data BExpr
   = BLit Bool
   | BRef Var
   | OpB  B_B  BExpr
   | OpBB BB_B BExpr BExpr
   | OpIB NN_B IExpr IExpr
  deriving (Eq,Generic,Ord,Read,Show,Typeable)

-- | Integer expressions with variable references.
data IExpr
   = ILit Int
   | IRef Var
   | OpI  N_N  IExpr
   | OpII NN_N IExpr IExpr
  deriving (Eq,Generic,Ord,Read,Show,Typeable)

-- Read in conditions as boolean expressions.
instance Read Cond where
  readsPrec i = map (\(e,s) -> (Cond e Nothing,s)) . readsPrec i

-- Use handy Boolean type class for boolean expressions.
instance Boolean BExpr where
  true  = BLit True
  false = BLit False
  bnot  = OpB Not
  (&&&) = OpBB And
  (|||) = OpBB Or
  (<+>) = OpBB XOr
  (==>) = OpBB Imp
  (<=>) = OpBB Eqv

-- The cache of conditions combined using this interface is always empty.
instance Boolean Cond where
  true  = Cond true Nothing
  false = Cond false Nothing
  bnot (Cond e _) = Cond (bnot e) Nothing
  (Cond l _) &&& (Cond r _) = Cond (l &&& r) Nothing
  (Cond l _) ||| (Cond r _) = Cond (l ||| r) Nothing
  (Cond l _) <+> (Cond r _) = Cond (l <+> r) Nothing
  (Cond l _) ==> (Cond r _) = Cond (l ==> r) Nothing
  (Cond l _) <=> (Cond r _) = Cond (l <=> r) Nothing

-- Use Num type class for integer arithmetic.
instance Num IExpr where
  fromInteger = ILit . fromInteger
  abs    = OpI Abs
  negate = OpI Neg
  signum = OpI Sign
  (+)    = OpII Add
  (-)    = OpII Sub
  (*)    = OpII Mul

-- Other integer arithmetic primitives.
instance PrimN IExpr where
  (./) = OpII Div
  (.%) = OpII Mod

-- Integer comparison primitives.
instance Prim BExpr IExpr where
  (.<)  = OpIB LT
  (.<=) = OpIB LTE
  (.==) = OpIB Equ
  (./=) = OpIB Neq
  (.>=) = OpIB GTE
  (.>)  = OpIB GT

-- Construct references with string literals
instance IsString Cond where
  fromString s = Cond (fromString s) Nothing
instance IsString BExpr where
  fromString = BRef . pack
instance IsString IExpr where
  fromString = IRef . pack


--
-- * Variation
--

-- | Variational values implemented as formula choice trees.
data V a
   = One a
   | Chc Cond (V a) (V a)
  deriving (Eq,Generic,Ord,Read,Show,Typeable)

-- | Variational optional values.
type VOpt a = V (Maybe a)

-- | Variational errors.
type VError = VOpt Error

-- | Variational optional value.
type Value = VOpt PVal

instance Functor V where
  fmap f (One v)     = One (f v)
  fmap f (Chc d l r) = Chc d (fmap f l) (fmap f r)

instance Applicative V where
  pure = One
  One f     <*> v = fmap f v
  Chc d f g <*> v = Chc d (f <*> v) (g <*> v)

instance Monad V where
  One v     >>= f = f v
  Chc d l r >>= f = Chc d (l >>= f) (r >>= f)


--
-- * Functions and expressions
--

-- | Named and primitively typed parameters.
data Param = Param {
  paramName :: Var,   -- ^ parameter name
  paramType :: PType  -- ^ parameter type
} deriving (Eq,Generic,Ord,Read,Show,Typeable)

-- | Unary functions.
data Fun = Fun Param (V Expr)
  deriving (Eq,Generic,Ord,Read,Show,Typeable)

-- | Expressions.
data Expr
   = Ref Var                             -- ^ variable reference
   | Res Path                            -- ^ resource reference
   | Lit (V PVal)                        -- ^ primitive literal
   | P1  Op1 (V Expr)                    -- ^ primitive unary function
   | P2  Op2 (V Expr) (V Expr)           -- ^ primitive binary function
   | P3  Op3 (V Expr) (V Expr) (V Expr)  -- ^ conditional expression
  deriving (Eq,Generic,Ord,Read,Show,Typeable)


instance Boolean Expr where
  true     = Lit (One (B True))
  false    = Lit (One (B False))
  bnot e   = P1 (B_B Not) (One e)
  e &&& e' = P2 (BB_B And) (One e) (One e')
  e ||| e' = P2 (BB_B Or)  (One e) (One e')
  e <+> e' = P2 (BB_B XOr) (One e) (One e')
  e ==> e' = P2 (BB_B Imp) (One e) (One e')
  e <=> e' = P2 (BB_B Eqv) (One e) (One e')

instance Boolean (V Expr) where
  true     = One (Lit (One (B True)))
  false    = One (Lit (One (B False)))
  bnot e   = One (P1 (B_B Not) e)
  e &&& e' = One (P2 (BB_B And) e e')
  e ||| e' = One (P2 (BB_B Or)  e e')
  e <+> e' = One (P2 (BB_B XOr) e e')
  e ==> e' = One (P2 (BB_B Imp) e e')
  e <=> e' = One (P2 (BB_B Eqv) e e')

-- Use Num type class for arithmetic.
instance Num Expr where
  fromInteger i = Lit (One (I (fromInteger i)))
  abs n    = P1 (N_N Abs) (One n)
  negate n = P1 (N_N Neg) (One n)
  signum n = P1 (N_N Sign) (One n)
  n + n'   = P2 (NN_N Add) (One n) (One n')
  n - n'   = P2 (NN_N Sub) (One n) (One n')
  n * n'   = P2 (NN_N Mul) (One n) (One n')

instance Num (V Expr) where
  fromInteger i = One (fromInteger i)
  abs n    = One (P1 (N_N Abs) n)
  negate n = One (P1 (N_N Neg) n)
  signum n = One (P1 (N_N Sign) n)
  n + n'   = One (P2 (NN_N Add) n n')
  n - n'   = One (P2 (NN_N Sub) n n')
  n * n'   = One (P2 (NN_N Mul) n n')

-- Other numeric arithmetic primitives.
instance PrimN Expr where
  n ./ n' = P2 (NN_N Div) (One n) (One n')
  n .% n' = P2 (NN_N Mod) (One n) (One n')

instance PrimN (V Expr) where
  n ./ n' = One (P2 (NN_N Div) n n')
  n .% n' = One (P2 (NN_N Mod) n n')

-- Numeric comparison primitives.
instance Prim Expr Expr where
  n .<  n' = P2 (NN_B LT)  (One n) (One n')
  n .<= n' = P2 (NN_B LTE) (One n) (One n')
  n .== n' = P2 (NN_B Equ) (One n) (One n')
  n ./= n' = P2 (NN_B Neq) (One n) (One n')
  n .>= n' = P2 (NN_B GTE) (One n) (One n')
  n .>  n' = P2 (NN_B GT)  (One n) (One n')

instance Prim (V Expr) (V Expr) where
  n .<  n' = One (P2 (NN_B LT)  n n')
  n .<= n' = One (P2 (NN_B LTE) n n')
  n .== n' = One (P2 (NN_B Equ) n n')
  n ./= n' = One (P2 (NN_B Neq) n n')
  n .>= n' = One (P2 (NN_B GTE) n n')
  n .>  n' = One (P2 (NN_B GT)  n n')


--
-- * Models
--

-- | A dictionary maps component names to their models.
type Dictionary = Env Name Model

-- | An application model.
data Model = Model [Param] Block
  deriving (Eq,Generic,Ord,Read,Show,Typeable)

-- | Statement block.
type Block = [Stmt]

-- | Statement in an application model.
data Stmt
   = Do Path Effect           -- ^ apply an effect
   | If (V Expr) Block Block  -- ^ conditional statement
   | In Path Block            -- ^ do work in a sub-environment
   | Let Var (V Expr) Block   -- ^ extend the variable environment
   | Load (V Expr) [V Expr]   -- ^ load a sub-model or profile
  deriving (Eq,Generic,Ord,Read,Show,Typeable)

-- | An effect on a particular resource.
data Effect
   = Create (V Expr)
   | Check  Fun
   | Modify Fun
   | Delete
  deriving (Eq,Generic,Ord,Read,Show,Typeable)


--
-- * Errors
--

-- | Errors that can occur during evaluation.
data Error
   = ResNotFound ResID
     -- ^ Resource not found in resource environment.
   | VarNotFound Var
     -- ^ Variable not found in variable environment.
   | CompNotFound Name
     -- ^ Component not found in dictionary.
   | CannotNormalize Path
     -- ^ Relative path cannot be normalized to an absolute path.
   | ArgTypeError Param Value
     -- ^ Type error on argument passed when loading a model.
   | PrimTypeError1 Op1 PVal
     -- ^ Type error applying a unary primitive operator.
   | PrimTypeError2 Op2 PVal PVal
     -- ^ Type error applying a binary primitive operator.
   | PrimTypeError3 Op3 PVal PVal PVal
     -- ^ Type error applying a ternary primitive operator.
   | EffectError EffectErrorKind Effect ResID Value
     -- ^ Error applying an effect.
   | StmtError StmtErrorKind Stmt PVal
     -- ^ Error executing a statement.
   | SolverError String
     -- ^ Error encountered during SAT solving.
  deriving (Eq,Generic,Ord,Read,Show,Typeable)

-- | Kinds of errors that can occur when resolving or combining an effect.
data EffectErrorKind
   = CheckFailure         -- ^ The condition of a check effect failed.
   | CheckTypeError       -- ^ Type error when applying a check effect.
   | CreateAlreadyExists  -- ^ Tried to create a resource where one already exists.
  deriving (Eq,Generic,Ord,Read,Show,Typeable)

-- | Kinds of errors that can occur in statements.
data StmtErrorKind
   = IfTypeError    -- ^ non-boolean condition
   | LoadTypeError  -- ^ not a component ID
  deriving (Eq,Generic,Ord,Read,Show,Typeable)
