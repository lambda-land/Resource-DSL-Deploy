{-# LANGUAGE
      DeriveGeneric,
      MultiParamTypeClasses
  #-}

module DSL.Predicate where

import Prelude hiding (LT,GT)

import Data.Set (Set)
import qualified Data.Set as Set

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

-- | Boolean predicates with variable references.
data Pred
     = BLit Bool
     | BRef Var
     | OpB  B_B  Pred
     | OpBB BB_B Pred Pred
     | OpIB II_B IExpr IExpr
  deriving (Eq,Generic,Show)

-- | Integer expressions with variable references.
data IExpr
     = ILit Int
     | IRef Var
     | OpI  I_I  IExpr
     | OpII II_I IExpr IExpr
  deriving (Eq,Generic,Show)

-- | The set of boolean variables referenced in a boolean predicate.
boolVars :: Pred -> Set Var
boolVars (BRef v)     = Set.singleton v
boolVars (OpB _ e)    = boolVars e
boolVars (OpBB _ l r) = boolVars l `Set.union` boolVars r
boolVars _            = Set.empty

-- | The set of integer variables referenced in a boolean predicate.
intVars :: Pred -> Set Var
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


-- ** Syntactic sugar

-- Use SBV's Boolean type class for boolean predicates.
instance Boolean Pred where
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
instance Prim Pred IExpr where
  (.<)  = OpIB LT
  (.<=) = OpIB LTE
  (.==) = OpIB Equ
  (.>=) = OpIB GTE
  (.>)  = OpIB GT


-- ** Substitution

-- | Substitute a boolean variable in a boolean predicate.
substB :: Var -> Bool -> Pred -> Pred
substB v b e@(BRef w)   = if w == v then BLit b else e
substB v b (OpB o e)    = OpB o (substB v b e)
substB v b (OpBB o l r) = OpBB o (substB v b l) (substB v b r)
substB _ _ e            = e

-- | Substitute an integer variable in a boolean predicate.
substI :: Var -> Int -> Pred -> Pred
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

-- | Evaluate a boolean predicate to either a ground or symbolic boolean,
--   given a corresponding dictionary of comparison operators and
--   environments binding all of the variables.
evalPred :: Prim b i => Env b -> Env i -> Pred -> b
evalPred _  _  (BLit b)     = fromBool b
evalPred mb _  (BRef v)     = envLookup' v mb
evalPred mb mi (OpB o e)    = opB_B o (evalPred mb mi e)
evalPred mb mi (OpBB o l r) = (opBB_B o `on` evalPred mb mi) l r
evalPred mb mi (OpIB o l r) = (opII_B o `on` evalIExpr mi) l r

-- | Evaluate an integer expression to either a ground or symbolic integer,
--   given an environment binding all of the variables.
evalIExpr :: PrimI i => Env i -> IExpr -> i
evalIExpr _ (ILit i)     = fromIntegral i
evalIExpr m (IRef v)     = envLookup' v m
evalIExpr m (OpI o e)    = opI_I o (evalIExpr m e)
evalIExpr m (OpII o l r) = (opII_I o `on` evalIExpr m) l r

-- | Evaluate a boolean predicate to a ground boolean.
predToBool :: Env Bool -> Env Int -> Pred -> Bool
predToBool = evalPred

-- | Evaluate a boolean predicate to a symbolic boolean.
predToSBool :: Env SBool -> Env SInt32 -> Pred -> SBool
predToSBool = evalPred

-- Enable satisfiability checking of predicates.
instance SAT Pred where
  toSymbolic e = do
    mb <- symEnv sBool (boolVars e)
    mi <- symEnv sInt32 (intVars e)
    return (predToSBool mb mi e)
