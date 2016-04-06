module Example.DemoDriver where

import Data.Aeson (decode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.SBV (sat)
import System.Directory (createDirectoryIfMissing)

import DSL.Expr
import DSL.Type
import DSL.Serialize
import DSL.Semantics
import DSL.Predicate
import DSL.SAT (toSymbolic)
import Example.Demo

-- | Write out example json input files.
writeInputs :: IO ()
writeInputs = do
  createDirectoryIfMissing False "inbox"
  B.writeFile "inbox/environment.json" (encodePretty initEnv)
  B.writeFile "inbox/requirement.json" (encodePretty reqType)

-- | Read inputs back in.
readInputs :: IO (Expr Refined, Schema Refined)
readInputs = do
  Just env <- fmap decode (B.readFile "inbox/environment.json")
  Just req <- fmap decode (B.readFile "inbox/requirement.json")
  return (env,req)

-- | Write out example json output file.
writeOutput :: IO ()
writeOutput = do
  createDirectoryIfMissing False "outbox"
  B.writeFile "outbox/environment.json" (encodePretty finalEnv)

-- | Run the demo driver:
--    * writes out some example input files
--    * reads them back in and prints out the corresponding Haskell values
--    * invokes the SMT solver to see if a predicate is satisfiable
--    * write an example output file
demoDriver :: IO ()
demoDriver = do
  writeInputs
  (env,req) <- readInputs
  putStrLn $ "Initial environment: " ++ show env
  putStrLn $ "Mission requirement: " ++ show req
  dummy <- sat (toSymbolic checkMe)
  putStrLn (show dummy)
  result <- runEvalIO (App gzip initEnv)
  putStrLn $ if result == finalEnv
    then "Result environment: " ++ show result
    else "Unexpected result: " ++ show result
  writeOutput
