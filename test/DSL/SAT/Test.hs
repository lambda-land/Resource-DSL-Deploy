module DSL.SAT.Test where

import Test.Tasty
import Test.Tasty.HUnit

import DSL.Boolean
import DSL.SAT
import DSL.Types


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
      equiv (BRef "A") (BRef "A") @?= True,
    testCase "A != B" $
      equiv (BRef "A") (BRef "B") @?= False,
    testCase "!(A | B) = !A & !B" $
      equiv (bnot (BRef "A" ||| BRef "B")) 
            ((bnot (BRef "A")) &&& (bnot (BRef "B"))) @?= True
  ]

testImplies = testGroup "implies"
  [
    testCase "A => A" $
      implies (BRef "A") (BRef "A") @?= True,
    testCase "A !=> B" $
      implies (BRef "A") (BRef "B") @?= False,
    testCase "A & B => A" $
      implies (BRef "A" &&& BRef "B") (BRef "A") @?= True,
    testCase "A !=> A & B" $
      implies (BRef "A") (BRef "A" &&& BRef "B") @?= False,
    testCase "A !=> A | B" $
      implies (BRef "A") (BRef "A" ||| BRef "B") @?= True
  ]

testNimplies = testGroup "nimplies"
  [
    testCase "!A =>! A" $
      nimplies (bnot (BRef "A")) (BRef "A") @?= True,
    testCase "!A !=>! A" $
      nimplies (bnot (BRef "A")) (bnot (BRef "A")) @?= False,
    testCase "!A !=>! B" $
      nimplies (bnot (BRef "A")) (BRef "B") @?= False,
    testCase "!A =>! A & B" $
      nimplies (bnot (BRef "A")) (BRef "A" &&& BRef "B") @?= True,
    testCase "!(A & B) !=>! A" $
      nimplies (bnot (BRef "A" &&& BRef "B")) (BRef "A") @?= False
  ]
