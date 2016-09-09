module DSL.Driver where

import Prelude hiding (init)

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Data.Aeson (parseJSON)
import Control.Monad (unless,void)
import Options.Applicative
import System.Environment (getArgs,withArgs)

import DSL.Model
import DSL.Profile
import DSL.Resource
import DSL.Serialize


--
-- * Run the Program
--

runDriver = getOptions >>= runWithOpts

runWithOpts opts = do
    dfus  <- readJSON (dictFile opts)
    init  <- readJSON (initFile opts)
    model <- readJSON (modelFile opts)
    let run r x = fmap snd (runWithDict (fmap Left dfus) r x)
    args <- case configValue opts of
              Just xs -> decodeJSON xs
              Nothing -> readJSON (configFile opts)
    out <- run init (loadModel model args)
    unless (noReqs opts) $ do
      reqs <- readJSON (reqsFile opts)
      void $ run out (loadProfile reqs [])
    writeJSON (outputFile opts) out


--
-- * Command Line Arguments
--

data CheckOpts = CheckOpts
     { noReqs      :: Bool
     , configValue :: Maybe String
     , dictFile    :: FilePath
     , initFile    :: FilePath
     , modelFile   :: FilePath
     , configFile  :: FilePath
     , outputFile  :: FilePath
     , reqsFile    :: FilePath }
  deriving (Data,Eq,Generic,Read,Show,Typeable)

data Command
     = Check   CheckOpts
     | Example Examples
  deriving (Data,Eq,Generic,Read,Show,Typeable)

data Examples
     = Location -- LocationOptions
  deriving (Data,Eq,Generic,Read,Show,Typeable)

checkOpts :: Parser CheckOpts
checkOpts = CheckOpts
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
      <> value "inbox/dictionary.json"
      <> help "Dictionary of DFU profiles" )
  
  <*> pathOption
       ( long "init-file"
      <> value "inbox/resources.json"
      <> help "Initial resource environment" )
  
  <*> pathOption
       ( long "model-file"
      <> value "inbox/model.json"
      <> help "Application model" )
  
  <*> pathOption
       ( long "config-file"
      <> value "inbox/configuration.json"
      <> help "Arguments to the application model" )
  
  <*> pathOption
       ( long "reqs-file"
      <> value "inbox/requirements.json"
      <> help "Mission requirements profile" )
  
  <*> pathOption
       ( long "output-file"
      <> value "outbox/resources.json"
      <> help "Final resource environment" )
  where
    pathOption mods = strOption (mods <> showDefault <> metavar "FILE")

getOptions :: IO CheckOpts
getOptions = getArgs >>= handleParseResult . execParserPure pref desc
  where
    pref = prefs (columns 100)
    desc = info (helper <*> checkOpts) fullDesc
