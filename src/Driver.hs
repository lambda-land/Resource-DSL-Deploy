module Driver where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as B
import System.Directory

import DSL.Expr
import DSL.Type
import DSL.Serialize
import Example.Stub

-- | Write out example json input files.
writeInputs :: IO ()
writeInputs = do
  createDirectoryIfMissing False "inbox"
  B.writeFile "inbox/environment.json" (encodePretty initEnv)
  B.writeFile "inbox/requirement.json" (encodePretty reqType)

-- | Read inputs back in.
readInputs :: IO (Expr, Schema Refined)
readInputs = do
  Just env <- fmap decode (B.readFile "inbox/environment.json")
  Just req <- fmap decode (B.readFile "inbox/requirement.json")
  return (env,req)

-- | Run the stub driver:
--    * writes out some example input files
--    * reads them back in
--    * invokes the SMT solver to 
runDriver :: IO ()
runDriver = do
  writeInputs
  readInputs >>= putStrLn . show
