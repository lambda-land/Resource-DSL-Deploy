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
testExpression = testGroup "Expressions Tests"
  [ testGroup "evalExpr"

    [ testCase "Evaluate a Literal" $
      do out <- runExpr (Lit (I 3)) envEmpty
         let y = I 3
         y @=? out

    , testCase "Evaluate a Reference" $
      do out <- runExpr (Ref "x") envEmpty
         let y = Unit
         y @=? out

    , testCase "Absolute value of a positive num is just the num" $
      do out <- runExpr (P1 (I_I Abs) (Lit (I 1))) envEmpty
         let y = I 1
         y @=? out

    , testCase "Absolute value of a neg num is just the num" $
      do out <- runExpr (P1 (I_I Abs) (Lit (I (- 1)))) envEmpty
         let y = I 1
         y @=? out

    , testCase "Negating a positive creates a negative" $
      do out <- runExpr (P1 (I_I Neg) (Lit (I 1))) envEmpty
         let y = I (- 1)
         y @=? out

    , testCase "Negating a negative creates a negative" $
      do out <- runExpr (P1 (I_I Neg) (Lit (I (- 1)))) envEmpty
         let y = I 1
         y @=? out

    , testCase "Negating zero is zero" $
      do out <- runExpr (P1 (I_I Neg) (Lit (I 0))) envEmpty
         let y = I 0
         y @=? out

    , testCase "signum of zero is zero" $
      do out <- runExpr (P1 (I_I Sign) (Lit (I 0))) envEmpty
         let y = I 0
         y @=? out

    , testCase "signum of a positive is 1" $
      do out <- runExpr (P1 (I_I Sign) (Lit (I 1729))) envEmpty
         let y = I 1
         y @=? out

    , testCase "signum of a negative is -1" $
      do out <- runExpr (P1 (I_I Sign) (Lit (I (- 1729)))) envEmpty
         let y = I (-1)
         y @=? out
    ]
  ]
