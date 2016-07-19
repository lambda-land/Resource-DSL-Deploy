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

import DSL.Env
import DSL.Primitive
import DSL.SAT


--
-- * Predicate Expressions
--

-- ** Abstract syntax

-- | Boolean predicates with variable references.
data BPred
     = BLit Bool
     | BRef Var
     | OpB  B_B  BPred
     | OpBB BB_B BPred BPred
     | OpIB II_B IPred IPred
  deriving (Eq,Generic,Show)

-- | Predicates on integers with variable references.
data IPred
     = ILit Int
     | IRef Var
     | OpI  I_I  IPred
     | OpII II_I IPred IPred
  deriving (Eq,Generic,Show)

-- | The set of boolean variables referenced in a boolean predicate.
boolVars :: BPred -> Set Var
boolVars (BRef v)     = Set.singleton v
boolVars (OpB _ e)    = boolVars e
boolVars (OpBB _ l r) = boolVars l `Set.union` boolVars r
boolVars _            = Set.empty

-- | The set of integer variables referenced in a boolean predicate.
intVars :: BPred -> Set Var
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
instance Boolean BPred where
  true  = BLit True
  false = BLit False
  bnot  = OpB Not
  (&&&) = OpBB And
  (|||) = OpBB Or
  (<+>) = OpBB XOr
  (==>) = OpBB Imp
  (<=>) = OpBB Eqv

-- Use Num type class for integer arithmetic.
instance Num IPred where
  fromInteger = ILit . fromInteger
  abs    = OpI Abs
  negate = OpI Neg
  signum = OpI Sign
  (+)    = OpII Add
  (-)    = OpII Sub
  (*)    = OpII Mul
  
-- Other integer arithmetic primitives.
instance PrimI IPred where
  (./) = OpII Div
  (.%) = OpII Mod

-- Integer comparison primitives.
instance Prim BPred IPred where
  (.<)  = OpIB LT
  (.<=) = OpIB LTE
  (.==) = OpIB Equ
  (.>=) = OpIB GTE
  (.>)  = OpIB GT


-- ** Substitution

-- | Substitute a boolean variable in a boolean predicate.
substB :: Var -> Bool -> BPred -> BPred
substB v b e@(BRef w)   = if w == v then BLit b else e
substB v b (OpB o e)    = OpB o (substB v b e)
substB v b (OpBB o l r) = OpBB o (substB v b l) (substB v b r)
substB _ _ e            = e

-- | Substitute an integer variable in a boolean predicate.
substI :: Var -> Int -> BPred -> BPred
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
evalBPred :: Prim b i => Env b -> Env i -> BPred -> b
evalBPred _  _  (BLit b)     = fromBool b
evalBPred mb _  (BRef v)     = envLookup' v mb
evalBPred mb mi (OpB o e)    = opB_B o (evalBPred mb mi e)
evalBPred mb mi (OpBB o l r) = (opBB_B o `on` evalBPred mb mi) l r
evalBPred mb mi (OpIB o l r) = (opII_B o `on` evalIPred mi) l r

-- | Evaluate an integer predicate to either a ground or symbolic integer,
--   given an environment binding all of the variables.
evalIPred :: PrimI i => Env i -> IPred -> i
evalIPred _ (ILit i)     = fromIntegral i
evalIPred m (IRef v)     = envLookup' v m
evalIPred m (OpI o e)    = opI_I o (evalIPred m e)
evalIPred m (OpII o l r) = (opII_I o `on` evalIPred m) l r

-- | Evaluate a boolean predicate to a ground boolean.
predToBool :: Env Bool -> Env Int -> BPred -> Bool
predToBool = evalBPred

-- | Evaluate a boolean predicate to a symbolic boolean.
predToSBool :: Env SBool -> Env SInt32 -> BPred -> SBool
predToSBool = evalBPred

-- Enable satisfiability checking of predicates.
instance SAT BPred where
  toSymbolic e = do
    mb <- symEnv sBool (boolVars e)
    mi <- symEnv sInt32 (intVars e)
    return (predToSBool mb mi e)
