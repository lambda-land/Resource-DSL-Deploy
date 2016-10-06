module Main where

import Test.Tasty

import DSL.Effect.Test

main :: IO ()
main = defaultMain $ testGroup ""
  [
    testResolveEffect
  ]
