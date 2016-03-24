module Driver where

import Data.Aeson (decode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.SBV (sat)
import System.Directory (createDirectoryIfMissing)

import DSL.Check
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

-- | Write out example json output file.
writeOutput :: IO ()
writeOutput = do
  createDirectoryIfMissing False "outbox"
  B.writeFile "outbox/environment.json" (encodePretty finalEnv)

-- | Run the stub driver:
--    * writes out some example input files
--    * reads them back in and prints out the corresponding Haskell values
--    * invokes the SMT solver to see if a predicate is satisfiable
--    * write an example output file
runDriver :: IO ()
runDriver = do
  writeInputs
  (env,req) <- readInputs
  putStrLn $ "Environment: " ++ show env
  putStrLn $ "Requirement: " ++ show req
  result <- sat (symPred [] ["m1","m2"] checkMe)
  putStrLn (show result)
  writeOutput
