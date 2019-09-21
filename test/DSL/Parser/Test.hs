module DSL.Parser.Test where

import Prelude hiding (GT, LT)

import Text.Megaparsec
import Data.Bifunctor (first)
import Data.Either
import Test.Tasty
import Test.Tasty.HUnit
import DSL.Types
import DSL.Parser

testParser = testGroup "DSL.Parser"
  [
    parseBool
    , parseInt
    , parseFloat
    , parseString
    , parseVar
    , parsePath
    , parseIExpr
    , parseBExpr
    , parsePVal
    , parseExpr
  ]

fails = isLeft

success = Right

lit = Lit . One

runp p = first errorBundlePretty . parse (topLevel p) ""

parseBool = testGroup "booleans"
  [
    testCase "true" $ runp bool "true" @?= success True,
    testCase "false" $ runp bool "false" @?= success False
  ]

parseInt = testGroup "ints"
  [
    testCase "1" $ runp int "1" @?= success 1,
    testCase "42" $ runp int "42" @?= success 42,
    testCase "abc" $ fails (runp int "abc") @? "abc fails"
  ]

parseFloat = testGroup "floats"
  [
    testCase "1.5" $ runp float "1.5" @?= success 1.5,
    testCase "33.0" $ runp float "33.0" @?= success 33.0,
    testCase "9.319664492078285e-4" $ runp float "9.319664492078285e-4" @?= success 9.319664492078285e-4,
    testCase "abc" $ fails (runp float "abc") @? "abc fails"
  ]

parseString = testGroup "string"
  [
    testCase "\"hello\"" $ runp stringLit "\"hello\"" @?= success "hello",
    testCase "hello" $ fails (runp stringLit "hello") @? "hello fails",
    testCase "\"a b c\"" $ runp stringLit "\"a b c\"" @?= success "a b c",
    testCase "\"_A,b-3?\"" $ runp stringLit "\"_A,b-3?\"" @?= success "_A,b-3?"
  ]

parseVar = testGroup "var"
  [
    testCase "x" $ runp var "x" @?= success "x",
    testCase "$x" $ runp var "$x" @?= success "$x",
    testCase "1a" $ fails (runp var "1a") @? "1a fails",
    testCase "true" $ fails (runp var "true") @? "no reserverd words"
  ]

parsePath = testGroup "path"
  [
    testCase "@/the/path" $ runp path "@/the/path" @?= success (Path Absolute ["the", "path"]),
    testCase "@." $ runp path "@." @?= success (Path Relative ["."]),
    testCase "@../the/path" $ runp path "@../the/path" @?= success (Path Relative ["..", "the", "path"])
  ]

parseIExpr = testGroup "iexpr"
  [
    testCase "42" $ runp iexpr "42" @?= success (ILit 42),
    testCase "$x" $ runp iexpr "$x" @?= success (IRef "$x"),
    testCase "abs 5" $ runp iexpr "abs 5" @?= success (OpI Abs (ILit 5)),
    testCase "signum 55" $ runp iexpr "signum 55" @?= success (OpI Sign (ILit 55)),
    testCase "-5" $ runp iexpr "-5" @?= success (OpI Neg (ILit 5)),
    testCase "$y*42" $ runp iexpr "$y*42" @?= success (OpII Mul (IRef "$y") (ILit 42)),
    testCase "(1+2)*3" $ runp iexpr "(1+2)*3" @?= success (OpII Mul (OpII Add (ILit 1) (ILit 2)) (ILit 3)),
    testCase "abs (2*$v)+57/-10" $ runp iexpr "abs (2*$v)+57/-10"
        @?=
      success
      (OpI Abs
        (OpII Add
          (OpII Mul
            (ILit 2)
            (IRef "$v")
          )
          (OpII Div
            (ILit 57)
            (OpI Neg (ILit 10))
          )
        )
      )
  ]

parseBExpr = testGroup "bexpr"
  [
    testCase "1>0" $ runp bexpr "1>0" @?= success (OpIB GT (ILit 1) (ILit 0)),
    testCase "($y*42)>5" $ runp bexpr "($y*42)>5" @?= success (OpIB GT (OpII Mul (IRef "$y") (ILit 42)) (ILit 5)),
    testCase "$x||true" $ runp bexpr "$x||true" @?= success (OpBB Or (BRef "$x") (BLit True)),
    testCase "false&&(!y)" $ runp bexpr "false&&(!y)" @?= success (OpBB And (BLit False) (OpB Not (BRef "y"))),
    testCase "0<=0" $ runp bexpr "0<=0" @?= success (OpIB LTE (ILit 0) (ILit 0)),
    testCase "$x<4" $ runp bexpr "$x<4" @?= success (OpIB LT (IRef "$x") (ILit 4)),
    testCase "$x <= 4" $ runp bexpr "$x <= 4" @?= success (OpIB LTE (IRef "$x") (ILit 4)),
    testCase "$x>=4" $ runp bexpr "$x>=4" @?= success (OpIB GTE (IRef "$x") (ILit 4)),
    testCase "$x<=4" $ runp bexpr "$x<=4" @?= success (OpIB LTE (IRef "$x") (ILit 4)),
    testCase "4<=$x" $ runp bexpr "4<=$x" @?= success (OpIB LTE (ILit 4) (IRef "$x")),
    testCase "($x<=4)||true" $ runp bexpr "($x<=4)||true" @?= success (OpBB Or (OpIB LTE (IRef "$x") (ILit 4)) (BLit True))
  ]

parsePVal = testGroup "pval"
  [
    testCase "()" $ runp pval "()" @?= success Unit,
    testCase "true" $ runp pval "true" @?= success (B True),
    testCase "false" $ runp pval "false" @?= success (B False),
    testCase "55" $ runp pval "55" @?= success (I 55),
    testCase "3.14" $ runp pval "3.14" @?= success (F 3.14),
    testCase "9.319664492078285e-4" $ runp pval "9.319664492078285e-4" @?= success (F 9.319664492078285e-4),
    testCase "\"abc\"" $ runp pval "\"abc\"" @?= success (S "abc")
  ]

parseExpr = testGroup "expr"
  [
    testCase "()" $ runp expr "()" @?= success (Lit (One Unit)),
    testCase "\"abc\"" $ runp expr "\"abc\"" @?= success (Lit (One (S "abc"))),
    testCase "false" $ runp expr "false" @?= success (Lit (One (B False))),
    testCase "9.319664492078285e-4" $ runp expr "9.319664492078285e-4" @?= success (Lit (One (F 9.319664492078285e-4))),
    testCase "[a]{(),false}" $ runp expr "[a]{(),false}" @?= success (Lit (Chc (BRef "a") (One Unit) (One (B False)))),
    testCase "[true||(1>a)]{55,3.14}" $ runp expr "[true||(1>a)]{55,3.14}" @?= success (Lit (Chc (OpBB Or (BLit True) (OpIB GT (ILit 1) (IRef "a"))) (One (I 55)) (One (F 3.14)))),
    testCase "[a]{[b]{1,2},3}" $ runp expr "[a]{[b]{1,2},3}" @?= success (Lit (Chc (BRef "a") (Chc (BRef "b") (One (I 1)) (One (I 2))) (One (I 3)))),
    testCase "@." $ runp expr "@." @?= success (Res (Path Relative ["."])),
    testCase "@/this/is/the/path" $ runp expr "@/this/is/the/path" @?= success (Res (Path Absolute ["this","is","the","path"])),
    testCase "$x" $ runp expr "$x" @?= success (Ref "$x"),
    testCase "unit ()" $ runp expr "unit ()" @?= success (P1 U_U (One (Lit (One Unit)))),
    testCase "unit [a]{1,2}" $ runp expr "unit [a]{1,2}" @?= success (P1 U_U (One (Lit (Chc (BRef "a") (One (I 1)) (One (I 2)))))),
    testCase "abs 5" $ runp expr "abs 5" @?= success (P1 (N_N Abs) (One (Lit (One (I 5))))),
    testCase "round 7.9" $ runp expr "round 7.9" @?= success (P1 (F_I Round) (One (Lit (One (F 7.9))))),
    testCase "if true then false else $var" $ runp expr "if true then false else $var" @?= success (P3 OpIf (One (Lit (One (B True)))) (One (Lit (One (B False)))) (One (Ref "$var"))),
    testCase "-5" $ runp expr "-5" @?= success (P1 (N_N Neg) (One (Lit (One (I 5))))),
    testCase "!true" $ runp expr "!true" @?= success (P1 (B_B Not) (One (Lit (One (B True))))),
    testCase "5*7" $ runp expr "5*7" @?= success (P2 (NN_N Mul) (One (Lit (One (I 5)))) (One (Lit (One (I 7))))),
    testCase "5/7" $ runp expr "5/7" @?= success (P2 (NN_N Div) (One (Lit (One (I 5)))) (One (Lit (One (I 7))))),
    testCase "5%7" $ runp expr "5%7" @?= success (P2 (NN_N Mod) (One (Lit (One (I 5)))) (One (Lit (One (I 7))))),
    testCase "5+7" $ runp expr "5+7" @?= success (P2 (NN_N Add) (One (Lit (One (I 5)))) (One (Lit (One (I 7))))),
    testCase "5-7" $ runp expr "5-7" @?= success (P2 (NN_N Sub) (One (Lit (One (I 5)))) (One (Lit (One (I 7))))),
    testCase "5<7" $ runp expr "5<7" @?= success (P2 (NN_B LT) (One (Lit (One (I 5)))) (One (Lit (One (I 7))))),
    testCase "5>7" $ runp expr "5>7" @?= success (P2 (NN_B GT) (One (Lit (One (I 5)))) (One (Lit (One (I 7))))),
    testCase "1 && 2" $ runp expr "1 && 2" @?= success (P2 (BB_B And) (One (Lit (One (I 1)))) (One (Lit (One (I 2))))),
    testCase "$x&&@." $ runp expr "$x&&@." @?= success (P2 (BB_B And) (One (Ref "$x")) (One (Res (Path Relative ["."])))),
    testCase "!true&&false" $ runp expr "!true&&false" @?= success (P2 (BB_B And) (One (P1 (B_B Not) (One (Lit (One (B True)))))) (One (Lit (One (B False)))))
  ]
