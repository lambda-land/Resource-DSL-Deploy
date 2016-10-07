module DSL.Expression.Test where

import Control.Exception.Base (catch)
import Test.Tasty
import Test.Tasty.HUnit

import DSL.Effect
import DSL.Environment
import DSL.Expression
import DSL.Primitive
import DSL.Resource

-- ** Helper functions
testCases :: [Assertion] -> [TestTree]
testCases = zipWith (testCase . show) [1..]

runExpr :: Expr -> ResEnv -> IO PVal
runExpr epr env =  fst <$> runInEmptyContext env (evalExpr epr)

-- ** Tests
testExpression :: TestTree
testExpression = testGroup "testExpressions"
  [ testGroup "evalExpr"

    [ testCase "Evaluate a Literal" $
      do out <- runExpr (Lit (I 3)) envEmpty
         let y = I 3
         y @=? out

    , testCase "Evaluate a Reference" $
      do out <- runExpr (Ref "x") envEmpty
         let y = Unit
         y @=? out
    ]
  ]
