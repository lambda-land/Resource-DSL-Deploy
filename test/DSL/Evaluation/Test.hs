module DSL.Evaluation.Test where

import Test.Tasty
import Test.Tasty.HUnit

import DSL.Boolean
import DSL.Environment
import DSL.Evaluation
import DSL.Predicate
import DSL.Sugar
import DSL.Types


testEval = testGroup "DSL.Evaluation" [testEvalOp1,testIf]

runEmpty = runEvalM withNoDict (withResEnv envEmpty)

pbool :: Bool -> VOpt PVal
pbool = One . Just . B

pint :: Int -> VOpt PVal
pint = One . Just . I

testEvalOp1 = testGroup "evalExpr Op1"
    [ testCase "not true = false" $ do
        out <- runOp1 (B_B Not) true
        out @?= (pbool False, false)
    , testCase "not 1 = _" $ do
        out <- runOp1 (B_B Not) 1
        out @?= (One Nothing, true)
    , testCase "-A<1,-1> = A<-1,1>" $ do
        out <- runOp1 (N_N Neg) (Chc "A" 1 (-1))
        out @?= (Chc "A" (pint (-1)) (pint 1), false)
    , testCase "-A<1,True> = A<-1,_>" $ do
        out <- runOp1 (N_N Neg) (Chc "A" 1 true)
        out @?= (Chc "A" (pint (-1)) (One Nothing), bnot "A")
    ]
  where
    runOp1 o e = do
      (res, SCtx _ _ err _) <- runEmpty (evalExpr (P1 o e))
      return (res, shrinkBExpr err)


testIf = testGroup "execStmt If"
    [ testCase "if A<true,false> then 1 else 2" $ do
      out <- runStmt (If (chc "A" true false) [create "/R" (One 1)] [create "/R" (One 2)])
      out @?= envFromList [("/R", Chc "A" (pint 1) (pint 2))]
    ]
  where
    runStmt s = do
      (_, SCtx renv _ _ _) <- runEmpty (execStmt s)
      return renv
