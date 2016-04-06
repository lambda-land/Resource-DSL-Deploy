module Example.DemoDriver where

import Data.SBV (sat)

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
  writeJSON "inbox" "environment" initEnv
  writeJSON "inbox" "requirement" reqType

-- | Read inputs back in.
readInputs :: IO (Expr Refined, Schema Refined)
readInputs = do
  env <- readJSON "inbox" "environment"
  req <- readJSON "inbox" "requirement"
  return (env,req)

-- | Write out example json output file.
writeOutput :: IO ()
writeOutput = writeJSON "outbox" "environment" finalEnv

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
