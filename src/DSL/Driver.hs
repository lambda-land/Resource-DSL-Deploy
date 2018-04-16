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
import DSL.Parser (parseBExprString)
import DSL.V

import DSL.Example.Location
import DSL.Example.Network
import DSL.Example.CrossApp


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
      Example (CrossApp opts) -> runCrossApp opts

runCheck :: CheckOpts -> IO ()
runCheck opts = do
    d <- case selection opts of
            Just s -> case parseBExprString s of
              Right b -> return (Just b)
              Left e -> do
                putStrLn ("Could not parse selection:\n" ++ e)
                exitWith (ExitFailure 1)
            Nothing -> return Nothing
    dict'  <- readJSON (dictFile opts) asDictionary
    let dict = case d of
          Just d' -> selectDict d' dict'
          Nothing -> dict'
    init'  <- readJSON (initFile opts) asResEnv
    let init = case d of
          Just d' -> fmap (select d') init'
          Nothing -> init'
    model' <- readJSON (modelFile opts) asModel
    let model = case d of
          Just d' -> selectModel d' model'
          Nothing -> model'
    args' <- case configValue opts of
              Just xs -> decodeJSON xs asConfig
              Nothing -> readJSON (configFile opts) asConfig
    let args = map (One . Lit) (case d of
          Just d' -> map (select d') args'
          Nothing -> args')
    sctx <- runWithDict dict init (loadModel model args) `catchEffErr` (opts, 2,"Error executing application model ...")
    writeOutput (outputFile opts) sctx
    if noReqs opts then do
      writeError (errorFile opts) sctx
      writeSuccess (successFile opts) sctx
    else do
      reqs' <- readJSON (reqsFile opts) asProfile
      let reqs = case d of
                   Just d' -> selectProfile d' reqs'
                   Nothing -> reqs'
      let (e, sctx') = runWithDict' dict sctx (loadProfile reqs [])
      writeError (errorFile opts) sctx'
      writeSuccess (successFile opts) sctx'
      case e of
        (Left _) -> putStrLn "Requirements not satisfied ..." >> exitWith (ExitFailure 3)
        (Right _) -> return ()
    putStrLn "Success"

catchEffErr :: (Either a b, StateCtx) -> (CheckOpts, Int, String) -> IO StateCtx
catchEffErr (Left _, s) (opts, code,msg) = do
  writeError (errorFile opts) s
  writeSuccess (successFile opts) s
  putStrLn (msg)
  exitWith (ExitFailure code)
catchEffErr (Right _, s) _ = return s

selectDict :: BExpr -> Dictionary -> Dictionary
selectDict d dict = fmap (selectEntry d) dict

selectEntry :: BExpr -> Entry -> Entry
selectEntry d (ProEntry p) = ProEntry (selectProfile d p)
selectEntry d (ModEntry m) = ModEntry (selectModel d m)

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
     , selection   :: Maybe String
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
     | CrossApp CrossAppOpts
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
        (progDesc "Network / image provider example"))
    <> command "crossapp"
        (info (CrossApp <$> (helper <*> parseCrossAppOpts))
        (progDesc "Cross application dependencies example"))
     )


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

  <*> (optional . strOption)
       ( short 's'
      <> long "selection"
      <> metavar "STRING"
      <> help "A string representing a boolean expression. Selects variants to be executed." )

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
