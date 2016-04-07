module Example.LocationDriver where

import System.Environment (getArgs)
import System.Exit

import DSL.Check
import DSL.Expr
import DSL.Type
import DSL.Serialize
import DSL.Semantics
import Example.Location


-- | Write out example DFUs and environments.
writeExamples :: IO ()
writeExamples = do
  putStrLn "Writing example inputs ..."
  mapM_ (uncurry (writeJSON "location/req")) locationReqs
  mapM_ (uncurry (writeJSON "location/dfu")) locationDFUs
  mapM_ (uncurry (writeJSON "location/env")) locationEnvs

-- | Read in inputs, either using command line arguments, or via the inbox
--   if no arguments are specified.
readInputs :: IO (Schema Refined, Expr Refined, Expr Refined)
readInputs = do
  args <- getArgs
  case args of
    ["inbox"] -> do
      putStrLn "Getting inputs from inbox ..."
      req <- readJSON "inbox" "location-req"
      dfu <- readJSON "inbox" "location-dfu"
      env <- readJSON "inbox" "location-env"
      return (req,dfu,env)
    [r,d,e] -> do
      putStrLn "Loading inputs from command line ..."
      req <- readJSON "location/req" r
      dfu <- readJSON "location/dfu" d
      env <- readJSON "location/env" e
      return (req,dfu,env)
    _ -> usage >> exitWith (ExitFailure 1)

-- | Write out example json output file.
writeOutput :: Expr Refined -> IO ()
writeOutput out = do
  putStrLn "Writing resulting environment to outbox ..."
  writeJSON "outbox" "location-out" out

locationDriver :: IO ()
locationDriver = do
  writeExamples
  (req,dfu,env) <- readInputs
  result <- runEvalIO (app2 appModel dfu env)
  writeOutput result
  if checkRec result req
    then putStrLn ("OK (requirements satisfied):\n" ++ show result)
         >> exitSuccess
    else putStrLn ("BAD (requirements not satisfied):\n" ++ show result)
         >> exitWith (ExitFailure 2)

usage :: IO ()
usage = do
  putStrLn "Usage:"
  putStrLn "  > stack exec resource-dsl location inbox"
  putStrLn "      -- or --"
  putStrLn "  > stack exec resource-dsl location [req-name] [dfu-name] [env-name]"
  putStrLn ""
  putStrLn "JSON files placed in the inbox should be named:"
  putStrLn "  * inbox/location-req.json"
  putStrLn "  * inbox/location-dfu.json"
  putStrLn "  * inbox/location-env.json"
  putStrLn ""
  putStrLn "When specifying (req-name|dfu-name|env-name) use the names of files in"
  putStrLn "the directories location/(req|dfu|env), minus the .json extension."
  putStrLn "" 
  putStrLn "Some specific examples:"
  putStrLn "  > stack exec resource-dsl location location gps-android GPS-Sat+GPS-DEV"
  putStrLn "  > stack exec resource-dsl location saasm gps-saasm GPS-Sat+Ext-USB+Has-UI"
  putStrLn "  > stack exec resource-dsl location location gps-usb GPS-Sat+Has-UI"
