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
import DSL.Name
import DSL.Environment
import DSL.Effect
import DSL.Profile
import DSL.Resource
import DSL.Model

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

    , testCase "RoundTrip for Function with Res" $
      roundTrip "Fun with Res Path" (Fun (Param "x" TBool)
                                     (Res (Path Relative ["foo"]))) asFun

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

  , testGroup "Names and Paths roundTrips"
    [ testCase "Roundtrip for Names" $
      roundTrip "RoundTrip Name" "Name" asName

    , testCase "Roundtrip for Symbol" $
      roundTrip "RoundTrip Symbol" (Symbol "Name") asSymbol

    , testCase "Roundtrip for ResID" $
      roundTrip "RoundTrip ResID" (ResID ["Name"]) asResID

    , testCase "Roundtrip for Path, Absolute" $
      roundTrip "RoundTrip Path" (Path Absolute ["Name"]) asPath

    , testCase "Roundtrip for Path, Relative" $
      roundTrip "RoundTrip Path" (Path Relative ["Name"]) asPath
    ]

  , testGroup "Roundtrip Environments"
    [ testCase "RoundTrip for Var Environments" $
      roundTrip "RoundTrip Simple Envrionment with k = path, v = name"
      (envFromList [((Path Relative ["foo"]), ("Name"))])
      (asEnv asPath asName)

    , testCase "RoundTrip for Variable Environment" $
      roundTrip "RoundTrip Variable Environment"
      (envFromList [("x", (B True))
                   , ("a", (I 13))
                   , ("y", Unit)
                   , ("z", (S "symbol"))]) asVarEnv

    , testCase "RoundTrip for Resource Environment" $
      roundTrip "Roundtrip Resource Environment"
      (envFromList [(ResID ["x"], (B True))
                   , (ResID ["a"], (I 13))
                   , (ResID ["y"], Unit)
                   , (ResID ["z"], (S "symbol"))]) asResEnv

    , testCase "RoundTrip for Dictionaries with profile" $
      roundTrip "RoundTrip Dictionary"
      (envFromList [("Symbol"
                    , ProEntry $ profile [Param "x" TInt, Param "y" TBool
                                         , Param "z" TUnit, Param "s" TSymbol]
                    [(Path Absolute ["foo"], [Create (bnot true)])
                                 , (Path Relative ["bar"], [Create (8 .% 4)])
                                 , (Path Absolute ["foo"], [Delete])
                                 , (Path Relative ["bar"]
                                   , [Check (Fun (Param "x" TUnit) (1 + 1))
                                     , Create (Lit (I 1))])])])
      asDictionary

    , testCase "RoundTrip for Dictionaries with model" $
      roundTrip "RoundTrip Dictionary"
      (envFromList [("CompID"
                    , ModEntry $ Model [Param "x" TInt, Param "y" TBool
                                       , Param "z" TUnit , Param "s" TSymbol]
                      [Do (Path Absolute ["foo"])
                       (Create (Res (Path Relative ["foo", "bar"])))
                      , If (true ||| false) [Let "gambino" (Ref "worldstar")
                                            [In (Path Absolute ["bar"]) []]]
                        []])])
      asDictionary
    ]

  , testGroup "RoundTrip Effects"
    [ testCase "Roundtripping modify effect" $
      roundTrip "Roundtripping for modify"
      (Modify (Fun (Param "x" TUnit) (Lit (I 0)))) asEffect
    ]

  , testGroup "RoundTrip Blocks"
    [ testCase "Roundtripping block" $
      roundTrip "block" [Do (Path Absolute ["foo"]) (Create (Ref "x"))
                        , If (true &&& false)
                          [Do (Path Relative ["bar"]) (Create (Lit (I 6)))]
                          [Load (negate 31) [Lit (I 10)]]
                        , In (Path Absolute ["foo"])
                          [Let "y" (Res (Path Relative ["bar"]))
                           [Do (Path Relative ["bar"]) (Delete)]]
                        , For "x" (Lit (I 1000))
                          [Do (Path Relative ["bar"])
                           (Create (Res (Path Relative ["foo"])))]]
      asBlock
    ]
  ]
