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
    [ testCase "not true = false"
        $ runOp1 (B_B Not) true
        @?= (pbool False, false)
    , testCase "not 1 = _"
        $ runOp1 (B_B Not) 1
        @?= (One Nothing, true)
    , testCase "-A<1,-1> = A<-1,1>"
        $ runOp1 (N_N Neg) (Chc "A" 1 (-1))
        @?= (Chc "A" (pint (-1)) (pint 1), false)
    , testCase "-A<1,True> = A<-1,_>"
        $ runOp1 (N_N Neg) (Chc "A" 1 true)
        @?= (Chc "A" (pint (-1)) (One Nothing), bnot "A")
    ]
  where
    runOp1 o e = 
      let (res, SCtx _ _ err _) = runEmpty (evalExpr (P1 o e))
      in (res, shrinkBExpr err)


testIf = testGroup "execStmt If"
    [ testCase "if A<true,false> then 1 else 2"
      $ runStmt (If (chc "A" true false) [create "/R" (One 1)] [create "/R" (One 2)])
      @?= envFromList [("/R", Chc "A" (pint 1) (pint 2))]
    ]
  where
    runStmt s =
      let (_, SCtx renv _ _ _) = runEmpty (execStmt s)
      in renv
