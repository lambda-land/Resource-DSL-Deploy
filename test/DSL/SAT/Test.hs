module DSL.SAT.Test where

import Test.Tasty
import Test.Tasty.HUnit
import DSL.Types
import DSL.SAT
import Data.SBV (bnot, (&&&), (|||))

testSAT = testGroup "DSL.SAT" [testSat, testUnsat, testEquiv, testImplies, testNimplies]

testSat = testGroup "sat"
  [
    testCase "sat" $
      sat (BRef "A") @?= True
  ]

testUnsat = testGroup "unsat"
  [
    testCase "unsat" $
      unsat (BRef "A" &&& (bnot (BRef "A"))) @?= True
  ]

testEquiv = testGroup "equiv"
  [
    testCase "A = A" $
      BRef "A" |=| BRef "A" @?= True,
    testCase "A != B" $
      BRef "A" |=| BRef "B" @?= False,
    testCase "!(A | B) = !A & !B" $
      bnot (BRef "A" ||| BRef "B") |=| ((bnot (BRef "A")) &&& (bnot (BRef "B"))) @?= True
  ]

testImplies = testGroup "implies"
  [
    testCase "A => A" $
      BRef "A" |=>| BRef "A" @?= True,
    testCase "A !=> B" $
      BRef "A" |=>| BRef "B" @?= False,
    testCase "A & B => A" $
      (BRef "A" &&& BRef "B") |=>| BRef "A" @?= True,
    testCase "A !=> A & B" $
      BRef "A" |=>| (BRef "A" &&& BRef "B") @?= False,
    testCase "A !=> A | B" $
      BRef "A" |=>| (BRef "A" ||| BRef "B") @?= True
  ]

testNimplies = testGroup "nimplies"
  [
    testCase "!A =>! A" $
      bnot (BRef "A") |=>!| BRef "A" @?= True,
    testCase "!A !=>! A" $
      bnot (BRef "A") |=>!| bnot (BRef "A") @?= False,
    testCase "!A !=>! B" $
      bnot (BRef "A") |=>!| BRef "B" @?= False,
    testCase "!A =>! A & B" $
      bnot (BRef "A") |=>!| (BRef "A" &&& BRef "B") @?= True,
    testCase "!(A & B) !=>! A" $
      bnot (BRef "A" &&& BRef "B") |=>!| BRef "A" @?= False
  ]
