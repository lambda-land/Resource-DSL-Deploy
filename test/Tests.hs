module Main where

import Test.Tasty
import DSL.SAT.Test
import DSL.V.Test
import DSL.Primitive.Test
import DSL.Value.Test
import DSL.Expression.Test

main :: IO ()
main = defaultMain tests

tests = testGroup "Tests" [testSAT, testV, testPrim, testValue, testExpr]
