module DSL.Serialize.Test where

import Data.Aeson
import Data.Aeson.BetterErrors
import Data.Aeson.Encode.Pretty (encodePretty)

import Test.Tasty
import Test.Tasty.HUnit

import DSL.Serialize
import DSL.Primitive
import DSL.Expression

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
    [ testCase "Round Trip Param" $
      roundTrip "a Param" (Param "foo" TInt) asParam
    ]
  ]
