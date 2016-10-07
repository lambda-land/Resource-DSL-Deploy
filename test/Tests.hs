module Main where

import Test.Tasty

import DSL.Effect.Test
import DSL.Expression.Test

main :: IO ()
main = defaultMain $ testGroup ""
  [
    testResolveEffect
  , testExpression
  ]
