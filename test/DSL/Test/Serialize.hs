module DSL.Test.Serialize where

import Data.Aeson
import Data.Aeson.BetterErrors
import Data.Aeson.Encode.Pretty (encodePretty)

import Test.Tasty
import Test.Tasty.HUnit

import DSL.Serialize


roundTrip :: (Eq a, Show a, ToJSON a) => String -> a -> ParseIt a -> Assertion
roundTrip name val parser =
    case parse parser (encodePretty val) of
      Right res -> assertEqual ("roundTrip " ++ name) val res
      Left err  -> do
        putStrLn "Error parsing JSON:"
        printParseError err
        assertFailure ("roundTrip parse error " ++ name)
