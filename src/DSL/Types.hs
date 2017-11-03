{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}

module DSL.Types where

import Prelude hiding (LT, GT)

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Data.Typeable
import Data.String (IsString(..))
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import Data.SBV (Boolean(..),SBool,SInteger,SInt8,SInt16,SInt32,SInt64)
import qualified Data.SBV as SBV
import Data.Fixed (mod')

import DSL.Name


-- ENVIRONMENTS

-- | An environment is a map from keys to values.
newtype Env k v = Env { envAsMap :: Map k v }
  deriving (Eq,Show)

-- | Apply a function to the map that implements this environment.
envOnMap :: (Map a b -> Map c d) -> Env a b -> Env c d
envOnMap f (Env m) = Env (f m)

instance Functor (Env k) where
  fmap = envOnMap . fmap

-- | Error thrown when a name is not found in the environment.
data NotFound = forall k. (Eq k, Show k, Typeable k) => NotFound k [k]

instance Eq NotFound where
  (NotFound k ks) == (NotFound x xs)
    | Just k' <- cast x, Just ks' <- cast xs = k == k' && ks == ks'
    | otherwise = False

instance Show NotFound where
  show (NotFound k ks) = "NotFound " ++ show k ++ " " ++ show ks


-- PATHS

-- | A path is either absolute or relative.
data PathKind = Absolute | Relative
  deriving (Eq,Ord,Show)

-- | A path through a resource environment. Unlike resource IDs, paths may be
--   relative, may contain "special" path names (such as ".." to refer to the
--   parent node), and may be appended with other paths. The idea is that a
--   resource ID is a simple key in the resource environment, while a path is
--   an intermediate object that can be manipulated in the language.
data Path = Path PathKind [Name]
  deriving (Eq,Ord,Show)

-- | Error that can occur when converting paths.
data PathError
     = CannotNormalize Path
  deriving (Eq,Show)

instance IsString Path where
  fromString ('/':s) = Path Absolute (splitOn "/" s)
  fromString s       = Path Relative (splitOn "/" s)

-- | Resource IDs are (absolute) paths from the root of the
--   resource environment.
newtype ResID = ResID [Name]
  deriving (Eq,Monoid,Ord,Show)


-- PRIMITIVES

-- | Primitive base types.
data PType = TUnit | TBool | TInt | TFloat | TSymbol | TBottom
  deriving (Eq,Show)

-- | Primitive values.
data PVal
     = Unit
     | B Bool
     | I Int
     | F Double
     | S Symbol
     | PErr
  deriving (Eq,Show)

--
-- * Primitive operators
--

-- | Primitive unary operators organized by type.
data Op1
     = U_U        -- ^ noop that matches a unit value
     | B_B B_B    -- ^ unary boolean operator
     | N_N N_N    -- ^ unary numeric operator
     | F_I F_I    -- ^ unary float-to-integer operator
  deriving (Eq,Show)

-- | Primitive binary operators organized by type.
data Op2
     = BB_B BB_B  -- ^ binary boolean operator
     | NN_N NN_N  -- ^ binary numeric operator
     | NN_B NN_B  -- ^ numeric comparison operator
  deriving (Eq,Show)

-- | Primitive ternary operator.
data Op3 = Cond
  deriving (Eq,Show)

-- | Boolean negation.
data B_B = Not
  deriving (Eq,Show)

-- | Unary numeric operators.
data N_N = Abs | Neg | Sign
  deriving (Eq,Show)

-- | Unary float-to-integer operators.
data F_I = Ceil | Floor | Round
  deriving (Eq,Show)

-- | Binary boolean operators.
data BB_B = And | Or | XOr | Imp | Eqv
  deriving (Eq,Show)

-- | Binary numeric comparison operators.
data NN_B = LT | LTE | Equ | Neq | GTE | GT
  deriving (Eq,Show)

-- | Binary numeric arithmetic operators.
data NN_N = Add | Sub | Mul | Div | Mod
  deriving (Eq,Show)

-- | Type error applying primitive operator.
data PrimTypeError
     = ErrorOp1 Op1 PVal
     | ErrorOp2 Op2 PVal PVal
     | ErrorOp3 Op3 PVal PVal PVal
  deriving (Eq,Show)

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
  deriving (Eq,Show)

-- | Boolean expressions with variable references.
data BExpr
     = BLit Bool
     | BRef Var
     | OpB  B_B  BExpr
     | OpBB BB_B BExpr BExpr
     | OpIB NN_B IExpr IExpr
  deriving (Eq,Show)

-- | Integer expressions with variable references.
data IExpr
     = ILit Int
     | IRef Var
     | OpI  N_N  IExpr
     | OpII NN_N IExpr IExpr
  deriving (Eq,Show)

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



-- VARIATION

data V a = One a | Chc BExpr (V a) (V a)
  deriving (Eq,Show)

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

type Value = V PVal

type Mask = V (Maybe Error)

type VError = V Error


-- EXPRESSIONS

-- | Named and primitively typed parameters.
data Param = Param Var PType
  deriving (Show,Eq)

-- | Unary functions.
data Fun = Fun Param Expr
  deriving (Eq,Show)

-- | Expressions.
data Expr
     = Ref Var                 -- ^ variable reference
     | Res Path                -- ^ resource reference
     | Lit PVal                -- ^ primitive literal
     | P1  Op1 Expr            -- ^ primitive unary function
     | P2  Op2 Expr Expr       -- ^ primitive binary function
     | P3  Op3 Expr Expr Expr  -- ^ conditional expression
  deriving (Eq,Show)

-- | Type error caused by passing argument of the wrong type.
data ArgTypeError = ArgTypeError Param PVal
  deriving (Eq,Show)

-- Use SBV's Boolean type class for boolean predicates.
instance Boolean Expr where
  true  = Lit (B True)
  false = Lit (B False)
  bnot  = P1 (B_B Not)
  (&&&) = P2 (BB_B And)
  (|||) = P2 (BB_B Or)
  (<+>) = P2 (BB_B XOr)
  (==>) = P2 (BB_B Imp)
  (<=>) = P2 (BB_B Eqv)

-- Use Num type class for arithmetic.
instance Num Expr where
  fromInteger = Lit . I . fromInteger
  abs    = P1 (N_N Abs)
  negate = P1 (N_N Neg)
  signum = P1 (N_N Sign)
  (+)    = P2 (NN_N Add)
  (-)    = P2 (NN_N Sub)
  (*)    = P2 (NN_N Mul)

-- Other numeric arithmetic primitives.
instance PrimN Expr where
  (./) = P2 (NN_N Div)
  (.%) = P2 (NN_N Mod)

-- Numeric comparison primitives.
instance Prim Expr Expr where
  (.<)  = P2 (NN_B LT)
  (.<=) = P2 (NN_B LTE)
  (.==) = P2 (NN_B Equ)
  (./=) = P2 (NN_B Neq)
  (.>=) = P2 (NN_B GTE)
  (.>)  = P2 (NN_B GT)


-- EFFECTS

-- ** Type

-- | An effect on a particular resource.
data Effect
     = Create Expr
     | Check  Fun
     | Modify Fun
     | Delete
  deriving (Show,Eq)


-- ** Errors

-- | Kinds of errors that can occur when resolving or combining an effect.
data EffectErrorKind
     = CheckFailure
     | CheckTypeError
     | NoSuchResource
     | ResourceAlreadyExists
  deriving (Show,Eq)

-- | An error resulting from applying a resource effect.
data EffectError = EffectError {
     effectErrorEffect :: Effect,
     effectErrorKind   :: EffectErrorKind,
     effectErrorResID  :: ResID,
     effectErrorValue  :: Maybe PVal
} deriving (Show,Eq)


-- PROFILES

-- | Resource profile: a parameterized account of all of the resource effects
--   of a program or component.
data Profile = Profile [Param] (Env Path [Effect])
  deriving (Show,Eq)


-- MODELS

-- | An application model.
data Model = Model [Param] Block
  deriving (Show,Eq)

-- | Statement block.
type Block = [Stmt]

-- | Statement in an application model.
data Stmt
     = Do Path Effect       -- ^ apply an effect
     | If Expr Block Block  -- ^ conditional statement
     | In Path Block        -- ^ do work in a sub-environment
     | For Var Expr Block   -- ^ loop over indexed sub-environments
     | Let Var Expr Block   -- ^ extend the variable environment
     | Load Expr [Expr]     -- ^ load a sub-model or profile
  deriving (Eq,Show)

-- | Kinds of errors that can occur in statements.
data StmtErrorKind
     = IfTypeError    -- ^ non-boolean condition
     | ForTypeError   -- ^ non-integer range bound
     | LoadTypeError  -- ^ not a component ID
  deriving (Eq,Show)

-- | Errors in statements.
data StmtError = StmtError {
     stmtErrorStmt  :: Stmt,
     stmtErrorKind  :: StmtErrorKind,
     stmtErrorValue :: Value
} deriving (Eq,Show)


-- RESOURCES

-- | Dictionary entry.
data Entry
     = ProEntry Profile
     | ModEntry Model
  deriving (Eq,Show)

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
  mask :: Mask
} deriving (Show)

-- | Reader context for evaluation.
data Context = Ctx {
    prefix      :: ResID,      -- ^ resource ID prefix
    environment :: VarEnv,     -- ^ variable environment
    dictionary  :: Dictionary, -- ^ dictionary of profiles and models
    vCtx        :: BExpr       -- ^ current variational context
} deriving (Show)

-- | A class of monads for computations that affect a resource environment,
--   given an evaluation context, and which may throw/catch exceptions.
class (MonadError VError m, MonadReader Context m, MonadState StateCtx m)
  => MonadEval m
instance (MonadError VError m, MonadReader Context m, MonadState StateCtx m)
  => MonadEval m

-- | A specific monad for running MonadEval computations.
type EvalM a = StateT StateCtx (ReaderT Context (Except VError)) a


-- ERROR

data Error = EnvE (NotFound)
           | PathE (PathError)
           | PrimE (PrimTypeError)
           | ExprE (ArgTypeError)
           | EffE (EffectError)
    deriving (Eq,Show)
