module DSL.Driver where

import Prelude hiding (init)

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Options.Applicative
import System.Environment (getArgs)

import DSL.Model
import DSL.Profile
import DSL.Resource
import DSL.Serialize


--
-- * Run the Program
--

runDriver = getOptions >>= runWithOpts

runWithOpts opts = do
    dfus   <- readJSON (dict opts)
    init   <- readJSON (init opts)
    model  <- readJSON (model opts)
    config <- readJSON (config opts)
    reqs   <- readJSON (reqs opts)
    let run r x = fmap snd (runWithDict (fmap Left dfus) r x)
    out <- run init (loadModel model config)
    run out (loadProfile reqs [])
    writeJSON (output opts) out


--
-- * Command Line Arguments
--

data Options = Options {
     dict   :: FilePath,
     init   :: FilePath,
     model  :: FilePath,
     config :: FilePath,
     reqs   :: FilePath,
     output :: FilePath
} deriving (Data,Eq,Generic,Read,Show,Typeable)

opts :: Parser Options
opts = Options
  <$> pathOption (short 'd'
      <> long "dict"
      <> value "inbox/dictionary.json"
      <> help "Dictionary of DFU profiles")
  
  <*> pathOption (short 'i'
      <> long "init"
      <> value "inbox/resources.json"
      <> help "Initial resource environment")
  
  <*> pathOption (short 'm'
      <> long "model"
      <> value "inbox/model.json"
      <> help "Application model")
  
  <*> pathOption (short 'c'
      <> long "config"
      <> value "inbox/configuration.json"
      <> help "Arguments to application model")
  
  <*> pathOption (short 'r'
      <> long "reqs"
      <> value "inbox/requirements.json"
      <> help "Mission requirements profile")
  
  <*> pathOption (short 'o'
      <> long "output"
      <> value "outbox/resources.json"
      <> help "Final resource environment")
  where
    pathOption mods = strOption (mods <> showDefault <> metavar "FILE")

getOptions :: IO Options
getOptions = getArgs >>= handleParseResult . execParserPure pref desc
  where
    pref = prefs (columns 100)
    desc = info (helper <*> opts) fullDesc
