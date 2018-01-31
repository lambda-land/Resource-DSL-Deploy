module DSL.SAT where

import Control.Monad (liftM2)
import Data.SBV (Boolean(..),SBool,sInt32,sBool,Symbolic,isSatisfiable)
import System.IO.Unsafe (unsafePerformIO)
import DSL.Types
import DSL.Predicate


-- | A type class for types that can be converted to symbolic predicates
--   and checked by a SAT solver.
class Boolean b => SAT b where
  toSymbolic :: b -> Symbolic SBool

-- | Is the predicate satisfiable?
sat :: SAT b => b -> Bool
sat p = case unsafePerformIO (isSatisfiable Nothing (toSymbolic p)) of
  Just b  -> b
  Nothing -> error "sat: Timed out despite no set time limit."

-- | Is the predicate unsatisfiable?
unsat :: SAT b => b -> Bool
unsat = not . sat

-- | Is the predicate a tautology?
taut :: SAT b => b -> Bool
taut = unsat . bnot

-- | Are these predicates equivalent?
equiv :: SAT b => b -> b -> Bool
equiv a b = taut (a <=> b)

-- | Operator for equiv
(|=|) :: SAT b => b -> b -> Bool
(|=|) = equiv

-- | Does the first predicate imply the second?
implies :: SAT b => b -> b -> Bool
implies a b = taut (a ==> b)

-- | Operator for implies
(|=>|) :: SAT b => b -> b -> Bool
(|=>|) = implies

-- | Does the first predicate imply that the second is false?
nimplies :: SAT b => b -> b -> Bool
nimplies a b = taut (a ==> (bnot b))

-- | Operator for nimplies
(|=>!|) :: SAT b => b -> b -> Bool
(|=>!|) = nimplies

-- | Implication with operators flipped
(|<=|) :: SAT b => b -> b -> Bool
a |<=| b = b |=>| a

-- | Nimplies with the operators flipped
(|!<=|) :: SAT b => b -> b -> Bool
a |!<=| b = b |=>!| a


-- Instances

instance Boolean b => Boolean (Symbolic b) where
  true  = return true
  false = return false
  bnot  = fmap bnot
  (&&&) = liftM2 (&&&)
  (|||) = liftM2 (|||)

instance SAT SBool where
  toSymbolic = return

instance SAT (Symbolic SBool) where
  toSymbolic = id

instance SAT (Symbolic Bool) where
  toSymbolic = fmap fromBool

-- Enable satisfiability checking of boolean expressions.
instance SAT BExpr where
  toSymbolic e = do
    mb <- symEnv sBool (boolVars e)
    mi <- symEnv sInt32 (intVars e)
    return (toSBool mb mi e)
