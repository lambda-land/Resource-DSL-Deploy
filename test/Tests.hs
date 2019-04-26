module Main where

import Test.Tasty
import DSL.SAT.Test
import DSL.V.Test
import DSL.Primitive.Test
import DSL.Value.Test
--import DSL.Expression.Test
import DSL.Parser.Test
import DSL.Example.SwapDau.Test

main :: IO ()
main = defaultMain tests

tests = testGroup "Tests" [
    testSAT
    , testV
    , testPrim
    , testValue
    --, testExpr
    , testParser
    , testSwap
  ]
