module Main where

import Test.Tasty

import DSL.Effect.Test
import DSL.Expression.Test
import DSL.Model.Test
import DSL.Profile.Test
import DSL.Serialize.Test

main :: IO ()
main = defaultMain $ testGroup ""
  [testResolveEffect
  , testExpression
  , testModel
  , testProfile
  , testSerialize
  ]
