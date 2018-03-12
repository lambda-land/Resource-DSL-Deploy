module DSL.Driver where

import Prelude hiding (init)

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Data.Monoid ((<>))
import Options.Applicative
import System.Environment (getArgs)
import System.Exit
import Data.SBV (bnot)

import DSL.Types hiding (Check)
import DSL.Model
import DSL.Profile
import DSL.Resource
import DSL.Serialize

import DSL.Example.Location
import DSL.Example.Network


--
-- * Run the Program
--

runDriver :: IO ()
runDriver = do
    cmd <- getCommand
    case cmd of
      Check opts -> runCheck opts
      Example (Location opts) -> runLocation opts
      Example (Network opts)  -> runNetwork opts

runCheck :: CheckOpts -> IO ()
runCheck opts = do
    print "made it to here"
    print (show opts)
    dict  <- readJSON (dictFile opts) asDictionary
    print "made it this far"
    init  <- readJSON (initFile opts) asResEnv
    model <- readJSON (modelFile opts) asModel
    args' <- case configValue opts of
              Just xs -> decodeJSON xs asConfig
              Nothing -> readJSON (configFile opts) asConfig
    let args = map (One . Lit) args'
    sctx <- runWithDict dict init (loadModel model args) `catchEffErr` (opts, 2,"Error executing application model ...")
    writeOutput (outputFile opts) sctx
    if noReqs opts then do
      writeError (errorFile opts) sctx
      writeSuccess (successFile opts) sctx
    else do
      reqs <- readJSON (reqsFile opts) asProfile
      let (e, sctx') = runWithDict' dict sctx (loadProfile reqs [])
      writeError (errorFile opts) sctx'
      writeSuccess (successFile opts) sctx'
      case e of
        (Left _) -> putStrLn "Requirements not satisfied ...\n" >> exitWith (ExitFailure 3)
        (Right _) -> return ()
    putStrLn "Success\n"

catchEffErr :: (Either a b, StateCtx) -> (CheckOpts, Int, String) -> IO StateCtx
catchEffErr (Left _, s) (opts, code,msg) = do
  writeError (errorFile opts) s
  writeSuccess (successFile opts) s
  putStrLn (msg ++ "\n")
  exitWith (ExitFailure code)
catchEffErr (Right _, s) _ = return s

writeError :: FilePath -> StateCtx -> IO ()
writeError fp (SCtx _ _ e) = writeJSON fp e

writeSuccess :: FilePath -> StateCtx -> IO ()
writeSuccess fp (SCtx _ s _) = writeJSON fp (bnot s)

writeOutput :: FilePath -> StateCtx -> IO ()
writeOutput fp (SCtx o _ _) = writeJSON fp o

--
-- * Command Line Arguments
--

data Command
     = Check CheckOpts
     | Example Example
  deriving (Data,Eq,Generic,Read,Show,Typeable)

data CheckOpts = CheckOpts
     { noReqs      :: Bool
     , configValue :: Maybe String
     , dictFile    :: FilePath
     , initFile    :: FilePath
     , modelFile   :: FilePath
     , configFile  :: FilePath
     , reqsFile    :: FilePath
     , outputFile  :: FilePath
     , errorFile   :: FilePath
     , successFile :: FilePath }
  deriving (Data,Eq,Generic,Read,Show,Typeable)


data Example
     = Location LocationOpts
     | Network  NetworkOpts
  deriving (Data,Eq,Generic,Read,Show,Typeable)


getCommand :: IO Command
getCommand = getArgs >>= handleParseResult . execParserPure pref desc
  where
    pref = prefs (columns 100)
    desc = info (helper <*> parseCommand) fullDesc

parseCommand :: Parser Command
parseCommand = subparser
     ( command "check"
        (info (Check <$> (helper <*> parseCheckOpts))
        (progDesc ("Execute an application model on a given resource environment; "
          ++ "optionally check result against given mission requirements")))
    <> command "example"
        (info (Example <$> (helper <*> parseExample))
        (progDesc "Generate example inputs and put them in the inbox")) )


parseExample :: Parser Example
parseExample = subparser
     ( command "location"
        (info (Location <$> (helper <*> parseLocationOpts))
        (progDesc "Location provider example"))
    <> command "network"
        (info (Network <$> (helper <*> parseNetworkOpts))
        (progDesc "Network / image provider example")) )


parseCheckOpts :: Parser CheckOpts
parseCheckOpts = CheckOpts
  <$> switch
       ( short 'n'
      <> long "no-reqs"
      <> help "Don't check output environment against mission requirements" )

  <*> (optional . strOption)
       ( short 'c'
      <> long "config"
      <> metavar "STRING"
      <> help "Arguments to the application model; overrides --config-file if present" )

  <*> pathOption
       ( long "dict-file"
      <> value defaultDict
      <> help "Dictionary of DFU profiles" )

  <*> pathOption
       ( long "init-file"
      <> value defaultInit
      <> help "Initial resource environment" )

  <*> pathOption
       ( long "model-file"
      <> value defaultModel
      <> help "Application model" )

  <*> pathOption
       ( long "config-file"
      <> value defaultConfig
      <> help "Arguments to the application model" )

  <*> pathOption
       ( long "reqs-file"
      <> value defaultReqs
      <> help "Mission requirements profile" )

  <*> pathOption
       ( long "output-file"
      <> value defaultOutput
      <> help "Final resource environment" )
  <*> pathOption
       ( long "error-file"
       <> value defaultError
       <> help "Global error value")
  <*> pathOption
       ( long "success-context"
       <> value defaultCtx
       <> help "Global success context")
  where
    pathOption mods = strOption (mods <> showDefault <> metavar "FILE")
