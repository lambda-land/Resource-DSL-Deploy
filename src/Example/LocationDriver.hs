module Example.LocationDriver where

import DSL.Expr
import DSL.Type
import DSL.Serialize
import DSL.Semantics
import Example.Location


-- | Write out example DFUs and environments.
writeExamples :: IO ()
writeExamples = do
  mapM_ (uncurry (writeJSON "location/dfu")) locationDFUs
  mapM_ (uncurry (writeJSON "location/env")) locationEnvs
  mapM_ (uncurry (writeJSON "location/req")) locationReqs

-- | Read in inputs.
readInputs :: IO (Expr Refined, Expr Refined, Schema Refined)
readInputs = do
  dfu <- readJSON "inbox" "location-dfu"
  env <- readJSON "inbox" "location-env"
  req <- readJSON "inbox" "location-req"
  return (dfu,env,req)
