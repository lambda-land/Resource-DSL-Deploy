module DSL.Example.NewVerbsSaasm where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Control.Monad (when)
import Data.List (intercalate,subsequences)
import Options.Applicative

import DSL.Environment
import DSL.Expression
import DSL.Model
import DSL.Path
import DSL.Primitive
import DSL.Profile
import DSL.Resource
import DSL.Serialize
import DSL.Sugar


--
-- * Location Provider Example
--

-- ** Application model

-- | Location-provider application model. The input parameter is the ID of the
--   location provider to use.
appModel = Model [Param "provider" TSymbol]
    [ Load (Ref "provider") [] ]


-- ** DFUs

-- | A dictionary of all the location DFUs with associated names.
locationDFUs :: Dictionary
locationDFUs = profileDict
    [ ("LocationProviderSaasmSimulated", gpsSaasm) ]

-- | USB-based SAASM GPS.
gpsSaasm :: Model
gpsSaasm = Model []
    [ checkUnit "GpsSatelliteConstellation"
    , checkUnit "UsbResource"
    , createUnit "LocationProvider"
    ]


-- ** Initial environments

-- | All relevant initial environments for the location scenario.
locationEnvs :: [(String, ResEnv)]
locationEnvs = [(toID ps, toEnv ps) | ps <- tail (subsequences paths)]
  where
    toEnv = envFromList . map (\p -> (ResID p, Unit))
    toID  = intercalate "+" . map (intercalate ".")
    paths = [ ["GpsSatelliteConstellation"]
            , ["GpsRecieverEmbedded"]
            , ["UsbResource"]
            , ["FileSystemResource"]
            , ["BluetoothResource"]
            , ["UserInterface"]
            ]

-- | Lookup a location environment by ID.
lookupLocationEnv :: Monad m => String -> m ResEnv
lookupLocationEnv envID = case lookup envID locationEnvs of
    Just env -> return env
    Nothing -> fail $ "bad environment ID, try one of: \n" ++ ids
  where ids = intercalate "\n" (map fst locationEnvs)


-- ** Mission requirements

-- | Require location.
hasLocation :: Profile
hasLocation = toProfile $ Model [] [checkUnit "LocationProvider"]

-- | Require SAASM location.
hasSaasm :: Profile
hasSaasm = toProfile $ Model []
    [ checkUnit "Location"
    , In "Location" [checkUnit "SAASM"]
    ]

-- | All relevant mission requirements for the location scenario.
locationReqs :: [(String, Profile)]
locationReqs = [("location", hasLocation), ("saasm", hasSaasm)]


--
-- * Driver
--

-- ** Basic Tests

runLocationTest :: String -> Int -> IO ResEnv
runLocationTest initID dfuID = do
    init <- lookupLocationEnv initID
    snd <$> runWithDict locationDFUs init (loadModel appModel [Lit (I dfuID)])


-- ** Driver Plugin

data NewVerbsSaasmOpts = NewVerbsSaasmOpts
     { genDict  :: Bool
     , genModel :: Bool
     , genReqs  :: Bool
     , genInit  :: Maybe String }
  deriving (Data,Eq,Generic,Read,Show,Typeable)

parseNewVerbsSaasmOpts :: Parser NewVerbsSaasmOpts
parseNewVerbsSaasmOpts = NewVerbsSaasmOpts
  <$> switch
       ( long "dict"
      <> help "Generate DFU dictionary" )
  
  <*> switch
       ( long "model"
      <> help "Generate application model" )
  
  <*> switch
       ( long "reqs"
      <> help "Generate mission requirements" )

  <*> (optional . strOption)
       ( long "init"
      <> metavar "STRING"
      <> help ("Generate initial resource environment; valid strings: "
               ++ intercalate ", " (map fst locationEnvs)) )

runNewVerbsSaasm :: NewVerbsSaasmOpts -> IO ()
runNewVerbsSaasm opts = do
    when (genDict opts)  (writeJSON defaultDict locationDFUs)
    when (genModel opts) (writeJSON defaultModel appModel)
    when (genReqs opts)  (writeJSON defaultReqs hasLocation)
    case genInit opts of
      Just k  -> lookupLocationEnv k >>= writeJSON defaultInit
      Nothing -> return ()
