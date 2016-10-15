module Main where

import Test.Tasty

import DSL.Effect.Test
import DSL.Expression.Test
import DSL.Model.Test

main :: IO ()
main = defaultMain $ testGroup ""
  [
    testResolveEffect
  , testExpression
  , testModel
  ]
