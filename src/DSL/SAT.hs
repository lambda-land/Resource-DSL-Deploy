-- | This module provides a simple but inefficient interface to the SAT solver.
--   Each call to a function in this module creates a new instance of the SAT
--   solver which has significant overhead and also cannot exploit caching.
--   'DSL.Evaluation' contains a more efficient interface when inside the
--   evaluation monad.
module DSL.SAT where

import Data.SBV hiding (sat)
import System.IO.Unsafe (unsafePerformIO)

import DSL.Boolean
import DSL.Types
import DSL.Condition

-- | Find a variable assignment that satisfies the given boolean expression.
satResult :: BExpr -> IO SatResult
satResult e = satWith z3 (toSBoolFresh e)

-- | Find several different variable assignments that satisfy the given
--   boolean expression.
satResults :: Int -> BExpr -> IO AllSatResult
satResults i e = allSatWith (z3 {allSatMaxModelCount = Just i}) (toSBoolFresh e)

-- | Is the predicate satisfiable?
sat :: BExpr -> Bool
sat e = unsafePerformIO (isSatisfiable (toSBoolFresh e))

-- | Is the predicate unsatisfiable?
unsat :: BExpr -> Bool
unsat = not . sat

-- | Is the predicate a tautology?
taut :: BExpr -> Bool
taut = unsat . bnot

-- | Are these predicates equivalent?
equiv :: BExpr -> BExpr -> Bool
equiv a b = taut (a <=> b)

-- | Does the first predicate imply the second?
implies :: BExpr -> BExpr -> Bool
implies a b = taut (a ==> b)

-- | Does the first predicate imply that the second is false?
nimplies :: BExpr -> BExpr -> Bool
nimplies a b = taut (a ==> bnot b)
