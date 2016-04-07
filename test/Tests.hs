module Main where

import Test.Tasty

import DSL.Test.Env

main :: IO ()
main = defaultMain $ testGroup "All tests" 
  [ DSL.Test.Env.tests
  ]
