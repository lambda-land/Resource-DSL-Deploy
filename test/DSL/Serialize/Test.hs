module DSL.Serialize.Test where

import Data.Aeson
import Data.Aeson.BetterErrors
import Data.Aeson.Encode.Pretty (encodePretty)

import Test.Tasty
import Test.Tasty.HUnit

import DSL.Serialize
import DSL.Primitive
import DSL.Expression
import DSL.Path

-- ** Helper functions
roundTrip :: (Eq a, Show a, ToJSON a) => String -> a -> ParseIt a -> Assertion
roundTrip name val parser =
  case parse parser $ encodePretty val of
      Right res -> assertEqual ("roundTrip " ++ name) val res
      Left err  -> do
        putStrLn "Error parsing JSON:"
        printParseError err
        assertFailure ("roundTrip parse error " ++ name)

--

testSerialize :: TestTree
testSerialize = testGroup "Roundtripping for Serialize"
  [ testGroup "RoundTrip PTypes"
    [ testCase "Round Trip TUnit" $
      roundTrip "A TUnit" TUnit asPType

    , testCase "Round Trip TBool" $
      roundTrip "A Bool" TBool asPType

    , testCase "Round Trip TInt" $
      roundTrip "A Int" TInt asPType

    , testCase "Round Trip TSymbol" $
      roundTrip "A Int" TSymbol asPType
    ]

  , testGroup "RoundTrip PVals"
    [ testCase "Round Trip Unit" $
      roundTrip "A Unit" Unit asPVal

    , testCase "Round Trip Bool" $
      roundTrip "A Bool" (B True) asPVal

    , testCase "Round Trip Int" $
      roundTrip "A Int" (I 1) asPVal

    , testCase "Round Trip Bool" $
      roundTrip "A Symbol" (S "foo") asPVal
    ]

  , testGroup "RoundTrip for Params"
    [ testCase "Round Trip Param with an TInt" $
      roundTrip "a Param" (Param "foo" TInt) asParam

    , testCase "Round Trip Param with a TBool" $
      roundTrip "a Param" (Param "foo" TBool) asParam

    , testCase "Round Trip Param with a TUnit" $
      roundTrip "a Param" (Param "foo" TUnit) asParam

    , testCase "Round Trip Param with a TSymbol" $
      roundTrip "a Param" (Param "foo" TSymbol) asParam
    ]

  , testGroup "RoundTrip for Functions"
    [ testCase "RoundTrip for Function with Ref" $
      roundTrip "Fun with Var reference" (Fun (Param "x" TUnit) (Ref "y")) asFun

    , testCase "RoundTrip for Function with Res" $
      roundTrip "Fun with Res Path" (Fun (Param "x" TBool)
                                     (Res (Path Absolute ["foo"]))) asFun

    , testCase "RoundTrip for Function with Lit" $
      roundTrip "Fun with Literal" (Fun (Param "x" TInt) (Lit (B True))) asFun

    , testCase "RoundTrip for Function with unary func 1" $
      roundTrip "Fun with unary func 1" (Fun (Param "x" TUnit) (abs 4)) asFun

    , testCase "RoundTrip for Function with unary func 2" $
      roundTrip "Fun with unary func 2" (Fun (Param "x" TUnit)
                                         (negate 5)) asFun

    , testCase "RoundTrip for Function with unary func 3" $
      roundTrip "Fun with unary func 3" (Fun (Param "x" TUnit)
                                         (bnot false)) asFun

    , testCase "RoundTrip for Function with binary func 1" $
      roundTrip "Fun with binary func 1" (Fun (Param "x" TUnit)
                                         (true &&& false)) asFun

    , testCase "RoundTrip for Function with binary func 2" $
      roundTrip "Fun with binary func 2" (Fun (Param "x" TInt)
                                         (8 ./ 4)) asFun

    , testCase "RoundTrip for Function with binary func 3" $
      roundTrip "Fun with binary func 3" (Fun (Param "x" TBool)
                                         (8 .% 4)) asFun

    , testCase "RoundTrip for Function with ternary func" $
      roundTrip "Fun with ternary func" (Fun (Param "x" TBool)
                                         (P3 Cond
                                          (true ||| false)
                                          (negate 0)
                                          (bnot true))) asFun
    ]
  ]
