module Main where

import Test.Tasty

import DSL.Test.Effect

main :: IO ()
main = defaultMain $ testGroup ""
  [
    testResolveEffect
  ]
