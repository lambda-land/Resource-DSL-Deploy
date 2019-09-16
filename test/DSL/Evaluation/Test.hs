module DSL.Evaluation.Test where

import Test.Tasty
import Test.Tasty.HUnit

import DSL.Boolean
import DSL.Environment
import DSL.Evaluation
import DSL.Predicate
import DSL.Types


runOp1 :: Op1 -> V Expr -> (VOpt PVal, BExpr)
runOp1 o e = (res, shrinkBExpr err)
  where
    (res, SCtx _ err _) = runEvalM withNoDict (withResEnv envEmpty) (evalExpr (P1 o e))

testEval = testGroup "DSL.Evaluation" [testEvalOp1]

testEvalOp1 = testGroup "evalExpr Op1"
  [
    testCase "not true = false"
      $ runOp1 (B_B Not) true
      @?= (One (Just (B False)), false)
  , testCase "not 1 = _"
      $ runOp1 (B_B Not) 1
      @?= (One Nothing, true)
  , testCase "-A<1,-1> = A<-1,1>"
      $ runOp1 (N_N Neg) (Chc "A" 1 (-1))
      @?= (Chc "A" (One (Just (I (-1)))) (One (Just (I 1))), false)
  , testCase "-A<1,True> = A<-1,_>"
      $ runOp1 (N_N Neg) (Chc "A" 1 true)
      @?= (Chc "A" (One (Just (I (-1)))) (One Nothing), bnot "A")
  ]
