{-# LANGUAGE TemplateHaskell #-}

module DSL.Test.Env where

import Data.Either (isLeft)

import Test.Tasty.HUnit
import Test.Tasty.TH

import DSL.Env

tests = $(testGroupGenerator)


-- Each linear variable must be read exactly once.

case_simpleUse1 = Right 3
  @=? runEnv (addLinear "x" 3 (lookUse "x"))

case_simpleUse2 = Right 3
  @=? runEnv (addLinear "x" 3 (addLinear "y" 4 (lookUse "y" >> lookUse "x")))

case_simpleUse3 = Right 3
  @=? runEnv (addLinear "y" 4 (addLinear "x" 3 (lookUse "y" >> lookUse "x")))

case_errorUse1 = assertBool "lookup linear variable as non-linear"
  $ isLeft $ runEnv (addLinear "x" 3 (lookRef "x"))

case_errorUse2 = assertBool "lookup undefined linear variable"
  $ isLeft $ runEnv (addLinear "x" 3 (lookUse "y"))

case_errorUse3 = assertBool "lookup linear variable multiple times"
  $ isLeft $ runEnv (addLinear "x" 3 (lookUse "x" >> lookUse "x"))

case_errorUse4 = assertBool "doesn't consume linear variable"
  $ isLeft $ runEnv (addLinear "x" 3 (addLinear "y" 4 (lookUse "x")))

case_errorUse5 = assertBool "doesn't consume linear variable"
  $ isLeft $ runEnv (addLinear "x" 3 (addLinear "y" 4 (lookUse "y")))


-- Non-linear variables may be read multiple times.

case_simpleRef1 = Right 3
  @=? runEnv (addLocal "x" 3 (lookRef "x"))

case_simpleRef2 = Right 3
  @=? runEnv (addLocal "x" 3 (lookRef "x" >> lookRef "x"))

case_simpleRef3 = Right 3
  @=? runEnv (addLocal "x" 3 (addLocal "y" 4 (lookRef "x")))

case_simpleRef4 = Right 3
  @=? runEnv (addLocal "y" 4 (addLocal "x" 3 (lookRef "x")))

case_errorRef1 = assertBool "lookup non-linear variable as linear"
  $ isLeft $ runEnv (addLocal "x" 3 (lookUse "x"))

case_errorRef2 = assertBool "lookup undefined non-linear variable"
  $ isLeft $ runEnv (addLocal "x" 3 (lookRef "y"))


-- TODO: test shadowing
