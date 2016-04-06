module Example.LocationDriver where

import DSL.Expr
import DSL.Type
import DSL.Serialize
import DSL.Semantics
import Example.Location


-- | Write out example DFUs.
writeDFUs :: IO ()
writeDFUs = mapM_ (uncurry (writeJSON "location/dfu")) locationDFUs

-- | Write out all possible environments.
writeEnvs :: IO ()
writeEnvs = mapM_ (uncurry (writeJSON "location/init")) allEnvs

-- | Read in inputs.
readInputs :: IO (Expr Refined, Expr Refined, Schema Refined)
readInputs = do
  dfu <- readJSON "inbox" "location-dfu"
  env <- readJSON "inbox" "environment"
  req <- readJSON "inbox" "requirement"
  return (dfu,env,req)
