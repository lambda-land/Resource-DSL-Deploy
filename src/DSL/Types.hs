{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}

module DSL.Types where

import Prelude hiding (LT, GT)

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Data.Data
import Data.String (IsString(..))
import Data.Text (pack, splitOn)
import Data.Map.Strict (Map)
import Data.SBV (Boolean(..),SBool,SInteger,SInt8,SInt16,SInt32,SInt64)
import qualified Data.SBV as SBV
import Data.Fixed (mod')
import Data.Text
import Data.Set (Set)

import DSL.Name


-- ENVIRONMENTS

-- | An environment is a map from keys to values.
newtype Env k v = Env { envAsMap :: Map k v }
  deriving (Eq,Show,Data,Read,Functor,Foldable)

type VEnv k v = Env k (VOpt v)

-- | Apply a function to the map that implements this environment.
envOnMap :: (Map a b -> Map c d) -> Env a b -> Env c d
envOnMap f (Env m) = Env (f m)

-- | Error thrown when a name is not found in the environment.
data NotFound = forall k. (Eq k, Show k, Typeable k) => NotFound k [k]

instance Eq NotFound where
  (NotFound k ks) == (NotFound x xs)
    | Just k' <- cast x, Just ks' <- cast xs = k == k' && ks == ks'
    | otherwise = False

instance Show NotFound where
  show (NotFound k ks) = "NotFound (" ++ show k ++ ") (" ++ show ks ++ ")"


-- PATHS

-- | A path is either absolute or relative.
data PathKind = Absolute | Relative
  deriving (Eq,Data,Read,Ord,Show)

-- | A path through a resource environment. Unlike resource IDs, paths may be
--   relative, may contain "special" path names (such as ".." to refer to the
--   parent node), and may be appended with other paths. The idea is that a
--   resource ID is a simple key in the resource environment, while a path is
--   an intermediate object that can be manipulated in the language.
data Path = Path PathKind [Name]
  deriving (Eq,Data,Read,Ord,Show)

-- | Error that can occur when converting paths.
data PathError
     = CannotNormalize Path
  deriving (Eq,Data,Read,Show)

instance IsString Path where
  fromString ('/':s) = Path Absolute (splitOn "/" (pack s))
  fromString s       = Path Relative (splitOn "/" (pack s))

-- | Resource IDs are (absolute) paths from the root of the
--   resource environment.
newtype ResID = ResID [Name]
  deriving (Eq,Data,Read,Monoid,Ord,Show)

instance IsString ResID where
  fromString ('/':s) = ResID (splitOn "/" (pack s))
  fromString s       = ResID (splitOn "/" (pack s))


-- PRIMITIVES

-- | Primitive base types.
data PType = TUnit | TBool | TInt | TFloat | TSymbol
  deriving (Eq,Data,Read,Show)

-- | Primitive values.
data PVal
     = Unit
     | B Bool
     | I Int
     | F Double
     | S Symbol
  deriving (Eq,Data,Show,Read)

instance IsString PVal where
  fromString = S . fromString

--
-- * Primitive operators
--

-- | Primitive unary operators organized by type.
data Op1
     = U_U        -- ^ noop that matches a unit value
     | B_B B_B    -- ^ unary boolean operator
     | N_N N_N    -- ^ unary numeric operator
     | F_I F_I    -- ^ unary float-to-integer operator
  deriving (Eq,Data,Read,Show)

-- | Primitive binary operators organized by type.
data Op2
     = BB_B BB_B  -- ^ binary boolean operator
     | NN_N NN_N  -- ^ binary numeric operator
     | NN_B NN_B  -- ^ numeric comparison operator
     | SS_B SS_B  -- ^ symbol comparison operator
  deriving (Eq,Data,Read,Show)

-- | Primitive ternary operator.
data Op3 = Cond
  deriving (Eq,Data,Read,Show)

-- | Boolean negation.
data B_B = Not
  deriving (Eq,Data,Read,Show)

-- | Unary numeric operators.
data N_N = Abs | Neg | Sign
  deriving (Eq,Data,Read,Show)

-- | Unary float-to-integer operators.
data F_I = Ceil | Floor | Round
  deriving (Eq,Data,Read,Show)

-- | Binary boolean operators.
data BB_B = And | Or | XOr | Imp | Eqv
  deriving (Eq,Data,Read,Show)

-- | Binary numeric comparison operators.
data NN_B = LT | LTE | Equ | Neq | GTE | GT
  deriving (Eq,Data,Read,Show)

-- | Binary numeric arithmetic operators.
data NN_N = Add | Sub | Mul | Div | Mod
  deriving (Eq,Data,Read,Show)

data SS_B = SEqu
  deriving (Eq,Data,Read,Show)

-- | Type error applying primitive operator.
data PrimTypeError
     = ErrorOp1 Op1 PVal
     | ErrorOp2 Op2 PVal PVal
     | ErrorOp3 Op3 PVal PVal PVal
  deriving (Eq,Data,Read,Show)

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

-- Symbolic instances

instance PrimN SInteger where
  (./)  = SBV.sDiv
  (.%)  = SBV.sMod

instance PrimN SInt8 where
  (./)  = SBV.sDiv
  (.%)  = SBV.sMod

instance PrimN SInt16 where
  (./)  = SBV.sDiv
  (.%)  = SBV.sMod

instance PrimN SInt32 where
  (./)  = SBV.sDiv
  (.%)  = SBV.sMod

instance PrimN SInt64 where
  (./)  = SBV.sDiv
  (.%)  = SBV.sMod

instance Prim SBool SInteger where
  (.<)  = (SBV..<)
  (.<=) = (SBV..<=)
  (.==) = (SBV..==)
  (./=) = (SBV../=)
  (.>=) = (SBV..>=)
  (.>)  = (SBV..>)

instance Prim SBool SInt8 where
  (.<)  = (SBV..<)
  (.<=) = (SBV..<=)
  (.==) = (SBV..==)
  (./=) = (SBV../=)
  (.>=) = (SBV..>=)
  (.>)  = (SBV..>)

instance Prim SBool SInt16 where
  (.<)  = (SBV..<)
  (.<=) = (SBV..<=)
  (.==) = (SBV..==)
  (./=) = (SBV../=)
  (.>=) = (SBV..>=)
  (.>)  = (SBV..>)

instance Prim SBool SInt32 where
  (.<)  = (SBV..<)
  (.<=) = (SBV..<=)
  (.==) = (SBV..==)
  (./=) = (SBV../=)
  (.>=) = (SBV..>=)
  (.>)  = (SBV..>)

instance Prim SBool SInt64 where
  (.<)  = (SBV..<)
  (.<=) = (SBV..<=)
  (.==) = (SBV..==)
  (./=) = (SBV../=)
  (.>=) = (SBV..>=)
  (.>)  = (SBV..>)


-- PREDICATES

-- | Unary boolean predicates.
data Pred
     = UPred            -- ^ trivial predicate on unit value
     | BPred Var BExpr  -- ^ predicate on boolean value
     | IPred Var BExpr  -- ^ predicate on integer value
  deriving (Eq,Data,Read,Show)

-- | Boolean expressions with variable references.
data BExpr
     = BLit Bool
     | BRef Var
     | OpB  B_B  BExpr
     | OpBB BB_B BExpr BExpr
     | OpIB NN_B IExpr IExpr
  deriving (Eq,Data,Read,Show)

-- | Integer expressions with variable references.
data IExpr
     = ILit Int
     | IRef Var
     | OpI  N_N  IExpr
     | OpII NN_N IExpr IExpr
  deriving (Eq,Data,Read,Show)

-- Use SBV's Boolean type class for boolean expressions.
instance Boolean BExpr where
  true  = BLit True
  false = BLit False
  bnot  = OpB Not
  (&&&) = OpBB And
  (|||) = OpBB Or
  (<+>) = OpBB XOr
  (==>) = OpBB Imp
  (<=>) = OpBB Eqv

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
instance IsString BExpr where
  fromString = BRef . pack
instance IsString IExpr where
  fromString = IRef . pack



-- VARIATION

data V a = One a | Chc BExpr (V a) (V a)
  deriving (Eq,Read,Data,Show,Typeable)

instance Functor V where
  fmap f (One v) = One . f $ v
  fmap f (Chc d l r) = Chc d (fmap f l) (fmap f r)

instance Applicative V where
  pure = One
  (One f) <*> v = fmap f v
  (Chc d f g) <*> v = Chc d (f <*> v) (g <*> v)

instance Monad V where
  (One v) >>= f = f v
  (Chc d l r) >>= f = Chc d (l >>= f) (r >>= f)

newtype VM m a = VM { unVM :: (m (VOpt a)) }

type VOpt a = V (Maybe a)

type SegList a = [Segment a]

data Segment a = Elems [a] | Split BExpr (SegList a) (SegList a)
  deriving (Eq,Data,Read,Show)

type VList a = [VOpt a]

type Value = VOpt PVal

type Mask = VOpt Error

type VType = V PType


-- EXPRESSIONS

-- | Named and primitively typed parameters.
data Param = Param Var VType
  deriving (Show,Data,Read,Eq)

-- | Unary functions.
data Fun = Fun Param (V Expr)
  deriving (Eq,Data,Read,Show)

-- | Expressions.
data Expr
     = Ref Var                             -- ^ variable reference
     | Res Path                            -- ^ resource reference
     | Lit (V PVal)                        -- ^ primitive literal
     | P1  Op1 (V Expr)                    -- ^ primitive unary function
     | P2  Op2 (V Expr) (V Expr)           -- ^ primitive binary function
     | P3  Op3 (V Expr) (V Expr) (V Expr)  -- ^ conditional expression
  deriving (Eq,Data,Read,Show)

class Pretty a where
  pretty :: a -> Text

data VEnvErr
   = NF NotFound
   | forall a k. (Eq a, Show a, Typeable a, Pretty a, Eq k, Show k, Typeable k)
     => VNF k BExpr (VOpt a)

instance Eq VEnvErr where
  (NF x) == (NF y) = x == y
  (VNF k b v) == (VNF k' b' v')
    | b == b', Just u' <- cast v', Just l' <- cast k' = v == u' && k == l'
    | otherwise = False
  _ == _ = False

instance Show VEnvErr where
  show (NF x) = "NF (" ++ show x ++ ")"
  show (VNF k b v) = "VNF (" ++ show k ++ ") (" ++ show b ++ ") (" ++ show v ++ ")"

-- | Type error caused by passing argument of the wrong type.
data ExprError = ArgTypeError Param Value PType PVal
               | VarNotFound VEnvErr
               | ResNotFound VEnvErr
  deriving (Eq,Show)

-- Use SBV's Boolean type class for boolean predicates.
instance Boolean (V Expr) where
  true  = One . Lit . One . B $ True
  false = One . Lit . One . B $ False
  bnot e  = One (P1 (B_B Not) e)
  e &&& e' = One (P2 (BB_B And) e e')
  e ||| e' = One (P2 (BB_B Or) e e')
  e <+> e' = One (P2 (BB_B XOr) e e')
  e ==> e' = One (P2 (BB_B Imp) e e')
  e <=> e' = One (P2 (BB_B Eqv) e e')

-- Use Num type class for arithmetic.
instance Num (V Expr) where
  fromInteger = One . Lit . One . I . fromInteger
  abs n   = One (P1 (N_N Abs) n)
  negate n = One (P1 (N_N Neg) n)
  signum n = One (P1 (N_N Sign) n)
  n + n'    = One (P2 (NN_N Add) n n')
  n - n'    = One (P2 (NN_N Sub) n n')
  n * n'    = One (P2 (NN_N Mul) n n')

-- Other numeric arithmetic primitives.
instance PrimN (V Expr) where
  n ./ n' = One (P2 (NN_N Div) n n')
  n .% n' = One (P2 (NN_N Mod) n n')

-- Numeric comparison primitives.
instance Prim (V Expr) (V Expr) where
  n .< n'  = One (P2 (NN_B LT) n n')
  n .<= n' = One (P2 (NN_B LTE) n n')
  n .== n' = One (P2 (NN_B Equ) n n')
  n ./= n' = One (P2 (NN_B Neq) n n')
  n .>= n' = One (P2 (NN_B GTE) n n')
  n .> n' = One (P2 (NN_B GT) n n')


-- EFFECTS

-- ** Type

-- | An effect on a particular resource.
data Effect
     = Create (V Expr)
     | Check  Fun
     | Modify Fun
     | Delete
  deriving (Show,Data,Read,Eq)


-- ** Errors

-- | Kinds of errors that can occur when resolving or combining an effect.
data EffectErrorKind
     = CheckFailure
     | CheckTypeError
     | NoSuchResource
     | ResourceAlreadyExists
  deriving (Show,Data,Read,Eq)

-- | An error resulting from applying a resource effect.
data EffectError = EffectError {
     effectErrorEffect :: Effect,
     effectErrorKind   :: EffectErrorKind,
     effectErrorResID  :: ResID,
     effectErrorValue  :: Maybe Value
} deriving (Show,Data,Read,Eq)


-- PROFILES

-- | Resource profile: a parameterized account of all of the resource effects
--   of a program or component.
data Profile = Profile [Param] (Env Path (SegList Effect))
  deriving (Show,Data,Read,Eq)


-- MODELS

-- | An application model.
data Model = Model [Param] Block
  deriving (Show,Eq,Data,Read)

-- | Statement block.
type Block = SegList Stmt

-- | Statement in an application model.
data Stmt
     = Do Path Effect       -- ^ apply an effect
     | If (V Expr) Block Block  -- ^ conditional statement
     | In Path Block        -- ^ do work in a sub-environment
     | For Var (V Expr) Block   -- ^ loop over indexed sub-environments
     | Let Var (V Expr) Block   -- ^ extend the variable environment
     | Load (V Expr) [V Expr]     -- ^ load a sub-model or profile
  deriving (Eq,Show,Data,Read)

-- | Kinds of errors that can occur in statements.
data StmtErrorKind
     = IfTypeError    -- ^ non-boolean condition
     | ForTypeError   -- ^ non-integer range bound
     | LoadTypeError  -- ^ not a component ID
  deriving (Eq,Show,Data,Read)

-- | Errors in statements.
data StmtError = StmtError {
     stmtErrorStmt  :: Stmt,
     stmtErrorKind  :: StmtErrorKind,
     stmtErrorValue :: PVal
} deriving (Eq,Show,Data,Read)


-- RESOURCES

-- | Dictionary entry.
data Entry
     = ProEntry Profile
     | ModEntry Model
  deriving (Eq,Show,Data,Read)

-- | Dictionary of profiles and models.
type Dictionary = Env CompID Entry

--
-- * Evaluation Monad
--

-- | Variable environment.
type VarEnv = Env Var Value

-- | Resource environment.
type ResEnv = Env ResID Value

data StateCtx = SCtx {
  renv :: ResEnv,
  errCtx :: BExpr,
  mask :: Mask
} deriving (Show,Eq)

-- | Reader context for evaluation.
data Context = Ctx {
    prefix      :: ResID,      -- ^ resource ID prefix
    environment :: VarEnv,     -- ^ variable environment
    dictionary  :: Dictionary, -- ^ dictionary of profiles and models
    vCtx        :: BExpr       -- ^ current variational context
} deriving (Show,Data,Read)

type MonadEval m = (MonadError Mask m, MonadReader Context m, MonadState StateCtx m)

-- | A specific monad for running MonadEval computations.
type EvalM a = ExceptT Mask (StateT StateCtx (Reader Context)) a


-- ERROR

data Error = EnvE (NotFound)
           | PathE (PathError)
           | PrimE (PrimTypeError)
           | ExprE (ExprError)
           | EffE (EffectError)
           | StmtE (StmtError)
    deriving (Eq,Show)

data SuccessCtx = SuccessCtx { ctx :: BExpr, cfgSpc :: Set Var }
  deriving (Eq,Show,Data,Read)
