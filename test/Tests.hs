module Main where

import Test.Tasty

import DSL.Evaluation.Test
import DSL.Example.SwapDau.Test
--import DSL.Expression.Test
import DSL.Parser.Test
import DSL.Primitive.Test
import DSL.SAT.Test
import DSL.Variational.Test

main :: IO ()
main = defaultMain tests

tests = testGroup "Tests"
    [ testSAT
    , testV
    , testPrim
    , testEval
    --, testExpr
    , testParser
    , testSwap
    ]
