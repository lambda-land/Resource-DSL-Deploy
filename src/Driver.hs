module Driver where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B

import DSL.Type
import DSL.Serialize
import Example.Stub

runDriver :: IO ()
runDriver = B.putStrLn (encode gzip)
