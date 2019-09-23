module DSL.SAT.Test where

import Test.Tasty
import Test.Tasty.HUnit

import DSL.Boolean
import DSL.Condition
import DSL.SAT
import DSL.Types


sat e = do
    z3 <- initSolver
    s <- runSat z3 (symBExprFresh e)
    runSat z3 (isSat s)

unsat    e   = fmap not (sat e)
taut     e   = unsat (bnot e)
equiv    a b = taut (a <=> b)
implies  a b = taut (a ==> b)
nimplies a b = taut (a ==> bnot b)

testSAT = testGroup "DSL.SAT" [testSat, testUnsat, testEquiv, testImplies, testNimplies]

testSat = testGroup "sat"
  [
    testCase "sat" $
      sat (BRef "A") >>= \b -> b @?=  True
  ]

testUnsat = testGroup "unsat"
  [
    testCase "unsat" $
      unsat (BRef "A" &&& (bnot (BRef "A"))) >>= \b -> b @?=  True
  ]

testEquiv = testGroup "equiv"
  [
    testCase "A = A" $
      equiv (BRef "A") (BRef "A") >>= \b -> b @?=  True,
    testCase "A != B" $
      equiv (BRef "A") (BRef "B") >>= \b -> b @?=  False,
    testCase "!(A | B) = !A & !B" $
      equiv (bnot (BRef "A" ||| BRef "B")) 
            ((bnot (BRef "A")) &&& (bnot (BRef "B"))) >>= \b -> b @?=  True
  ]

testImplies = testGroup "implies"
  [
    testCase "A => A" $
      implies (BRef "A") (BRef "A") >>= \b -> b @?=  True,
    testCase "A !=> B" $
      implies (BRef "A") (BRef "B") >>= \b -> b @?=  False,
    testCase "A & B => A" $
      implies (BRef "A" &&& BRef "B") (BRef "A") >>= \b -> b @?=  True,
    testCase "A !=> A & B" $
      implies (BRef "A") (BRef "A" &&& BRef "B") >>= \b -> b @?=  False,
    testCase "A !=> A | B" $
      implies (BRef "A") (BRef "A" ||| BRef "B") >>= \b -> b @?=  True
  ]

testNimplies = testGroup "nimplies"
  [
    testCase "!A =>! A" $
      nimplies (bnot (BRef "A")) (BRef "A") >>= \b -> b @?=  True,
    testCase "!A !=>! A" $
      nimplies (bnot (BRef "A")) (bnot (BRef "A")) >>= \b -> b @?=  False,
    testCase "!A !=>! B" $
      nimplies (bnot (BRef "A")) (BRef "B") >>= \b -> b @?=  False,
    testCase "!A =>! A & B" $
      nimplies (bnot (BRef "A")) (BRef "A" &&& BRef "B") >>= \b -> b @?=  True,
    testCase "!(A & B) !=>! A" $
      nimplies (bnot (BRef "A" &&& BRef "B")) (BRef "A") >>= \b -> b @?=  False
  ]
