module DSL.Driver where

import Prelude hiding (init)

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Control.Monad (unless,void)
import Control.Monad.Catch (catch)
import Options.Applicative
import System.Environment (getArgs)
import System.Exit

import DSL.Expression
import DSL.Model
import DSL.Profile
import DSL.Resource
import DSL.Serialize
import DSL.Pretty

--import DSL.Example.Location
--import DSL.Example.Network


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
    dict  <- readJSON (dictFile opts) asDictionary
    init  <- readJSON (initFile opts) asResEnv
    model <- readJSON (modelFile opts) asModel
    let run r x = fmap snd (runWithDict dict r x)
    args <- case configValue opts of
              Just xs -> decodeJSON xs asConfig
              Nothing -> readJSON (configFile opts) asConfig
    out <- run init (loadModel model (map Lit args))
      `catchEffErr` (2,"Error executing application model ...")
    writeJSON (outputFile opts) out
    unless (noReqs opts) $ do
      reqs <- readJSON (reqsFile opts) asProfile
      void (run out (loadProfile reqs []))
        `catchEffErr` (3,"Requirements not satisfied ...")
    putStrLn "OK"
  where
    catchEffErr x (code,msg) = catch x $ \err -> do
      putStrLn (msg ++ "\n" ++ prettyEffectError err)
      exitWith (ExitFailure code)


--
-- * Command Line Arguments
--

data Command
     = Check   CheckOpts
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
     , outputFile  :: FilePath }
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
  where
    pathOption mods = strOption (mods <> showDefault <> metavar "FILE")
