module DSL.Predicate where

import Prelude hiding (LT,GT)

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Data.Function (on)
import Data.SBV
import GHC.Generics (Generic)

import DSL.Environment
import DSL.Primitive
import DSL.SAT


--
-- * Predicate Expressions
--

-- ** Abstract syntax

-- | Variables.
type Var = Name

-- | Unary boolean predicates.
data Pred
     = UPred            -- ^ trivial predicate on unit value
     | BPred Var BExpr  -- ^ predicate on boolean value
     | IPred Var BExpr  -- ^ predicate on integer value
  deriving (Eq,Generic,Show)

-- | Boolean expressions with variable references.
data BExpr
     = BLit Bool
     | BRef Var
     | OpB  B_B  BExpr
     | OpBB BB_B BExpr BExpr
     | OpIB II_B IExpr IExpr
  deriving (Eq,Generic,Show)

-- | Integer expressions with variable references.
data IExpr
     = ILit Int
     | IRef Var
     | OpI  I_I  IExpr
     | OpII II_I IExpr IExpr
  deriving (Eq,Generic,Show)

-- | The set of boolean variables referenced in a boolean expression.
boolVars :: BExpr -> Set Var
boolVars (BRef v)     = Set.singleton v
boolVars (OpB _ e)    = boolVars e
boolVars (OpBB _ l r) = boolVars l `Set.union` boolVars r
boolVars _            = Set.empty

-- | The set of integer variables referenced in a boolean expression.
intVars :: BExpr -> Set Var
intVars (BLit _)     = Set.empty
intVars (BRef _)     = Set.empty
intVars (OpB _ e)    = intVars e
intVars (OpBB _ l r) = intVars l `Set.union` intVars r
intVars (OpIB _ l r) = intVars' l `Set.union` intVars' r
  where
    intVars' (ILit _)     = Set.empty
    intVars' (IRef v)     = Set.singleton v
    intVars' (OpI _ e)    = intVars' e
    intVars' (OpII _ l r) = intVars' l `Set.union` intVars' r

-- | Rename a variable in a expression.
renameVar :: Var -> Var -> BExpr -> BExpr
renameVar _   _   p@(BLit _)   = p
renameVar old new p@(BRef v)   = if v == old then BRef new else p
renameVar old new (OpB o e)    = OpB o (renameVar old new e)
renameVar old new (OpBB o l r) = OpBB o (renameVar old new l) (renameVar old new r)
renameVar old new (OpIB o l r) = OpIB o (renameVar' l) (renameVar' r)
  where
    renameVar' p@(ILit _)   = p
    renameVar' p@(IRef v)   = if v == old then IRef new else p
    renameVar' (OpI o e)    = OpI o (renameVar' e)
    renameVar' (OpII o l r) = OpII o (renameVar' l) (renameVar' r)


-- ** Syntactic sugar

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
instance PrimI IExpr where
  (./) = OpII Div
  (.%) = OpII Mod

-- Integer comparison primitives.
instance Prim BExpr IExpr where
  (.<)  = OpIB LT
  (.<=) = OpIB LTE
  (.==) = OpIB Equ
  (.>=) = OpIB GTE
  (.>)  = OpIB GT


-- ** Substitution

-- | Substitute a boolean variable in a boolean expression.
substB :: Var -> Bool -> BExpr -> BExpr
substB v b e@(BRef w)   = if w == v then BLit b else e
substB v b (OpB o e)    = OpB o (substB v b e)
substB v b (OpBB o l r) = OpBB o (substB v b l) (substB v b r)
substB _ _ e            = e

-- | Substitute an integer variable in a boolean expression.
substI :: Var -> Int -> BExpr -> BExpr
substI _ _ e@(BLit _)   = e
substI _ _ e@(BRef _)   = e
substI v i (OpB o e)    = OpB o (substI v i e)
substI v i (OpBB o l r) = OpBB o (substI v i l) (substI v i r)
substI v i (OpIB o l r) = OpIB o (substI' l) (substI' r)
  where
    substI' e@(ILit _)   = e
    substI' e@(IRef w)   = if w == v then ILit i else e
    substI' (OpI o e)    = OpI o (substI' e)
    substI' (OpII o l r) = OpII o (substI' l) (substI' r)


-- ** Evaluation to plain and symbolic values

-- | Construct an environment with fresh symbolic values for each variable.
symEnv :: (Name -> Symbolic b) -> Set Name -> Symbolic (Env b)
symEnv f s = fmap (Map.fromList . zip vs) (mapM f vs)
  where vs = Set.toList s

-- | Evaluate a boolean expression to either a ground or symbolic boolean,
--   given a corresponding dictionary of comparison operators and
--   environments binding all of the variables.
evalBExpr :: Prim b i => Env b -> Env i -> BExpr -> b
evalBExpr _  _  (BLit b)     = fromBool b
evalBExpr mb _  (BRef v)     = assumeFound (envLookup v mb)
evalBExpr mb mi (OpB o e)    = opB_B o (evalBExpr mb mi e)
evalBExpr mb mi (OpBB o l r) = (opBB_B o `on` evalBExpr mb mi) l r
evalBExpr mb mi (OpIB o l r) = (opII_B o `on` evalIExpr mi) l r

-- | Evaluate an integer expression to either a ground or symbolic integer,
--   given an environment binding all of the variables.
evalIExpr :: PrimI i => Env i -> IExpr -> i
evalIExpr _ (ILit i)     = fromIntegral i
evalIExpr m (IRef v)     = assumeFound (envLookup v m)
evalIExpr m (OpI o e)    = opI_I o (evalIExpr m e)
evalIExpr m (OpII o l r) = (opII_I o `on` evalIExpr m) l r

-- | Evaluate a boolean expression to a ground boolean.
toBool :: Env Bool -> Env Int -> BExpr -> Bool
toBool = evalBExpr

-- | Evaluate a boolean expression to a symbolic boolean.
toSBool :: Env SBool -> Env SInt32 -> BExpr -> SBool
toSBool = evalBExpr

-- Enable satisfiability checking of boolean expressions.
instance SAT BExpr where
  toSymbolic e = do
    mb <- symEnv sBool (boolVars e)
    mi <- symEnv sInt32 (intVars e)
    return (toSBool mb mi e)
