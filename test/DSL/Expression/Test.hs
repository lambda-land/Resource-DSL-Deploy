module DSL.Expression.Test where

import Test.Tasty
import Test.Tasty.HUnit
import DSL.Types
import DSL.Resource
import DSL.Environment
import DSL.Expression
import Data.SBV ((|||), (&&&))

testExpr = testGroup "DSL.Expression" [testEvalExpr]

runExpr :: VarEnv -> ResEnv -> Expr -> (Either VError (V (Maybe PVal)), StateCtx)
runExpr v r e = runInContext
                  (Ctx (ResID []) v envEmpty (BLit True))
                  (SCtx r (BLit False) (One Nothing))
                  (unVM . evalExpr $ e)

runExprEmpty :: Expr -> (Either VError (V (Maybe PVal)), StateCtx)
runExprEmpty = runExpr envEmpty envEmpty

testEvalExpr = testGroup "evalExpr" [testRefs, testLits]

testRefs = testGroup "Refs"
  [
    testCase "basic reference" $
      runExpr
        (envFromList [("x", (Chc (BRef "A") (One . Just $ (I 1)) (One . Just $ (B True))))])
        envEmpty
        (Ref "x")
      @?=
      (Right (Chc (BRef "A") (One . Just $ (I 1)) (One . Just $ (B True))), SCtx {renv = envEmpty, errCtx = BLit False, mask = One Nothing}),
    testCase "ref not in env" $
      runExpr
        (envFromList [("x", (Chc (BRef "A") (One . Just $ (I 1)) (One . Just $ (B True))))])
        envEmpty
        (Ref "y")
      @?= (Left . One . Just . ExprE . VarNotFound . NF $ NotFound ("y"::String) ["x"],SCtx {renv = envEmpty, errCtx = ((BLit False) ||| (BLit True)), mask = (Chc (BLit True) (One . Just . ExprE . VarNotFound . NF $ NotFound ("y"::String) ["x"]) (One Nothing)) }),
    testCase "ref not in some var envs" $
      runExpr
        (envFromList [("x", (Chc (BRef "A") (One Nothing) (One . Just $ (B True))))])
        envEmpty
        (Ref "x")
    @?=
    (Right (Chc (BRef "A") (One Nothing) (One . Just $ (B True))), SCtx {renv = envEmpty, errCtx = ((BLit False) ||| (BLit True &&& BRef "A")), mask = (Chc (BLit True &&& BRef "A") (One . Just . ExprE . VarNotFound $ VNF (BLit True &&& BRef "A") (Chc (BRef "A") (One Nothing) (One . Just $ (B True)))) (One Nothing)) })
  ]

testLits = testGroup "Literals"
  [
    testCase "Lit A<1,True> = A<1,True>" $
      runExprEmpty (Lit (Chc (BRef "A") (One (I 1)) (One (B True)))) @?=
      (Right (Chc (BRef "A") (One . Just $ (I 1)) (One . Just $ (B True))), SCtx {renv = envEmpty, errCtx = BLit False, mask = One Nothing})
  ]
