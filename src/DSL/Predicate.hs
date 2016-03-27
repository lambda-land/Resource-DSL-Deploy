{-# LANGUAGE DeriveGeneric #-}

module DSL.Predicate where

import Prelude hiding (LT,GT)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Function (on)
import Data.SBV
import GHC.Generics (Generic)

import DSL.Env
import DSL.SAT


--
-- * Predicate Expressions
--

-- ** Abstract syntax

-- | Binary boolean operators.
data OpBB = And | Or | XOr | Imp
  deriving (Eq,Generic,Show)

-- | Binary integer comparison operators.
data OpIB = LT | LTE | Equ | GTE | GT
  deriving (Eq,Generic,Show)

-- | Binary integer arithmetic operators.
data OpII = Add | Sub | Mul | Div
  deriving (Eq,Generic,Show)

-- | Boolean predicates with variable references.
data BPred
     = BLit Bool
     | BRef Var
     | BNot BPred
     | OpBB OpBB BPred BPred
     | OpIB OpIB IPred IPred
  deriving (Eq,Generic,Show)

-- | Predicates on integers with variable references.
data IPred
     = ILit Int
     | IRef Var
     | INeg IPred
     | OpII OpII IPred IPred
  deriving (Eq,Generic,Show)


-- | The set of boolean variables referenced in a boolean predicate.
boolVars :: BPred -> Set Var
boolVars (BRef v)     = Set.singleton v
boolVars (BNot e)     = boolVars e
boolVars (OpBB _ l r) = boolVars l `Set.union` boolVars r
boolVars _            = Set.empty

-- | The set of integer variables referenced in a boolean predicate.
intVars :: BPred -> Set Var
intVars (BLit _)     = Set.empty
intVars (BRef _)     = Set.empty
intVars (BNot e)     = intVars e
intVars (OpBB _ l r) = intVars l `Set.union` intVars r
intVars (OpIB _ l r) = intVars' l `Set.union` intVars' r
  where
    intVars' (ILit _)     = Set.empty
    intVars' (IRef v)     = Set.singleton v
    intVars' (INeg e)     = intVars' e
    intVars' (OpII _ l r) = intVars' l `Set.union` intVars' r


-- ** Syntactic sugar

-- Use SBV's Boolean type class for boolean predicates.
instance Boolean BPred where
  true  = BLit True
  false = BLit False
  bnot  = BNot
  (&&&) = OpBB And
  (|||) = OpBB Or
  (<+>) = OpBB XOr
  (==>) = OpBB Imp

-- Smart constructors for integer comparisons.
(@<), (@<=), (@==), (@>=), (@>) :: IPred -> IPred -> BPred
(@<)  = OpIB LT
(@<=) = OpIB LTE
(@==) = OpIB Equ
(@>=) = OpIB GTE
(@>)  = OpIB GT

infix 4 @<, @<=, @==, @>=, @>

-- Smart constructors for integer arithmetic operations.
(@+), (@-), (@*), (@/) :: IPred -> IPred -> IPred
(@+) = OpII Add
(@-) = OpII Sub
(@*) = OpII Mul
(@/) = OpII Div

infixl 6 @+, @-
infixl 7 @*, @/


-- ** Evaluation to plain and symbolic values

-- | Comparison and division operator dictionary to support working with both
--   ground and symbolic values.
data OpDict b i = OpDict {
  opLT, opLTE, opEqu, opGTE, opGT :: i -> i -> b,
  opDiv                           :: i -> i -> i
}

-- | Operator dictionary for ground values.
groundOps :: OpDict Bool Int
groundOps = OpDict (<) (<=) (==) (>=) (>) div

-- | Operator dictionary for symbolic values.
symOps :: (EqSymbolic i, OrdSymbolic i, SDivisible i) => OpDict SBool i
symOps = OpDict (.<) (.<=) (.==) (.>=) (.>) sDiv

-- | Evaluate a boolean predicate to either a ground or symbolic boolean,
--   given a corresponding dictionary of comparison operators and
--   environments binding all of the variables.
evalBPred :: (Boolean b, Num i)
          => OpDict b i -> Env b -> Env i -> BPred -> b
evalBPred _ _  _  (BLit b)     = fromBool b
evalBPred _ mb _  (BRef v)     = envLookup v mb
evalBPred d mb mi (BNot e)     = bnot (evalBPred d mb mi e)
evalBPred d mb mi (OpBB o l r) = (op o `on` evalBPred d mb mi) l r
  where
    op And = (&&&)
    op Or  = (|||)
    op XOr = (<+>)
    op Imp = (==>)
evalBPred d mb mi (OpIB o l r) = (op o `on` evalIPred d mi) l r
  where
    op LT  = opLT d
    op LTE = opLTE d
    op Equ = opEqu d
    op GTE = opGTE d
    op GT  = opGT d

-- | Evaluate an integer predicate to either a ground or symbolic integer,
--   given an environment binding all of the variables.
evalIPred :: Num i => OpDict b i -> Env i -> IPred -> i
evalIPred _ _ (ILit i)     = fromIntegral i
evalIPred _ m (IRef v)     = envLookup v m
evalIPred d m (INeg e)     = negate (evalIPred d m e)
evalIPred d m (OpII o l r) = (op o `on` evalIPred d m) l r
  where
    op Add = (+)
    op Sub = subtract
    op Mul = (*)
    op Div = opDiv d

-- | Evaluate a boolean predicate to a ground boolean.
predToBool :: Env Bool -> Env Int -> BPred -> Bool
predToBool = evalBPred groundOps

-- | Evaluate a boolean predicate to a symbolic boolean.
predToSBool :: (Num i, EqSymbolic i, OrdSymbolic i, SDivisible i)
            => Env SBool -> Env i -> BPred -> SBool
predToSBool = evalBPred symOps

-- Enable satisfiability checking of predicates.
instance SAT BPred where
  toSymbolic e = do
    mb <- symEnv sBool (boolVars e)
    mi <- symEnv sInteger (intVars e)
    return (predToSBool mb mi e)
