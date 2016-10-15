module DSL.Expression.Test where

import           Control.Exception.Base (catch)
import           Test.Tasty
import           Test.Tasty.HUnit

import           DSL.Effect
import           DSL.Environment
import           DSL.Expression
import           DSL.Primitive
import           DSL.Profile
import           DSL.Resource

-- ** Helper functions
testCases :: [Assertion] -> [TestTree]
testCases = zipWith (testCase . show) [1..]

runExpr :: Expr -> ResEnv -> IO PVal
runExpr epr env =  fst <$> runInEmptyContext env (evalExpr epr)

runCheck :: Param -> PVal -> ResEnv -> IO (Var, PVal)
runCheck param pval env = fst <$> runInEmptyContext env (checkArg param pval)

runEvalFun :: Fun -> PVal -> ResEnv -> IO PVal
runEvalFun f v env = fst <$> runInEmptyContext env (evalFun f v)

funEq :: Int -> Fun
funEq n = Fun (P "x" TInt) (Ref "x" .== Lit (I n))

funAddThree :: Fun
funAddThree = Fun (P "x" TInt) (Lit (I 3) + Ref "x")

-- ** Tests
testExpression :: TestTree
testExpression = testGroup "Expressions Tests"
  [ testGroup "evalExpr"

    [ testCase "Evaluate a Literal" $
      do out <- runExpr (Lit (I 3)) envEmpty
         let y = I 3
         y @=? out

    -- , testCase "Evaluate a Reference" $
    --   do out <- runExpr (Ref "x") envEmpty
    --      let y = envFromList [(["foo"], (Ref "x"))]
    --      y @=? out

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

    , testGroup "checkArgs"

      [ testCase "Checking Bools" $
        do out <- runCheck (P "x" TBool) (B True) envEmpty
           let y = ("x", B True)
           y @=? out

      , testCase "Checking Ints" $
        do out <- runCheck (P "x" TInt) (I 496) envEmpty
           let y = ("x", I 496)
           y @=? out

      , testCase "Checking Units" $
        do out <- runCheck (P "x" TUnit) Unit envEmpty
           let y = ("x", Unit)
           y @=? out
      ]

    , testGroup "evalFun"

      [ testCase "a Fun with Ints" $
        do out <- runEvalFun funAddThree (I 3) envEmpty
           let y = (I 6)
           y @=? out

      , testCase "a Fun with Refs" $
        do out <- runEvalFun (funEq 3) (I 3) envEmpty
           let y = (B True)
           y @=? out
      ]

      , testGroup "evalWithArgs" $

        [ testCase "how does withArgs work" $
        do out <- runInContext (Ctx [""] envEmpty
                                (envSingle "x"
                                 (ProEntry
                                  (Profile [P "x" TInt]
                                   (envSingle ["foo"] [Create (Ref "x")])))))
             envEmpty (withArgs
                       [(P "x" TInt)]
                       [Lit (I 3)]
                       (evalExpr (Ref "x")))   -- should return 3
           let y = (I 3)
           y @=? fst out
        ]
  ]
