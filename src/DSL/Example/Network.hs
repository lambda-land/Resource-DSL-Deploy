module DSL.Example.Network where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Control.Monad (when)
import Options.Applicative hiding (str)

import DSL.Types
import DSL.Environment
import DSL.Evaluation
import DSL.Serialize
import DSL.Sugar


--
-- * Network Provider Example
--

-- ** Application model

imageParams :: [Param]
imageParams =
    [ Param "imageRate" TInt
    , Param "resX"      TInt
    , Param "resY"      TInt
    , Param "color"     TBool
    , Param "scale"     TFloat
    , Param "compress"  TBool
    ]

saParams :: [Param]
saParams =
    [ Param "pliRate"   TInt  -- position location information
    -- , Param "logging" TBool
    ]

-- | Application model.
appModel :: Model
appModel = Model (Param "clients" TInt : imageParams ++ saParams)
    [ Load (str "image-producer") [imageRate, resX, resY, color, scale, compress]
    , Load (str "situational-awareness-producer") [pliRate]
    , modify "/Network/Bandwidth" TFloat
        (val - clients * clients * convert imageRate (One (Res "Image/Size")))
    , modify "/Network/Bandwidth" TFloat
        (val - clients * clients * convert pliRate (One (Res "SA/Size")))
    ]
  where
    convert rate size = (rate ./ (One (Lit (One (F 60.0))))) * (size ./ One (Lit (One (F 1000.0))))


-- ** DFUs

-- | A dictionary of all the network DFUs with associated names.
networkDFUs :: Dictionary
networkDFUs = envFromList
    [ ("image-producer", imageProducer)
    , ("image-producer-scale", imageScale)
    , ("image-producer-compress", imageCompress)
    , ("situational-awareness-producer", saProducer)
    ]

-- | Base image producer.
imageProducer :: Model
imageProducer = Model imageParams
    [ createUnit "Image"
    , In "Image"
      [ create "Rate"  imageRate
      , create "ResX"  resX
      , create "ResY"  resY
      , create "Color" color
      , create "Size"  (resX * resY * (One (color ?? (24,8))) ./ One (Lit (One (F 15.0)))) -- bits
      ]
    , Load (str "image-producer-scale") [scale]
    , If compress
      [ Load (str "image-producer-compress") [] ]
      []
    ]

-- | Image scaling DFU.
imageScale :: Model
imageScale = Model [Param "scale" TFloat]
    [ modify "Image/Size" TFloat (val * scale) ]
    -- [ In "Image"
    --   [ modify "ResX" TInt (val ./ scale)
    --   , modify "ResY" TInt (val ./ scale)
    --   , modify "Size" TInt (val ./ (scale * scale))
    --   ]
    -- , modify "CPU" TInt  -- modifies local CPU
    --     (val - Res "Image/ResX" * Res "Image/ResY")
    -- ]

-- | Image compression DFU.
imageCompress :: Model
imageCompress = Model []
    [ modify "Image/Size" TFloat (val ./ 2)
    -- , modify "CPU" TInt (val - Res "Image/ResX" * Res "Image/ResY")
    ]

-- | Situational awareness producer.
saProducer :: Model
saProducer = Model saParams
    [ createUnit "SA"
    , In "SA"
      [ create "Size" (400 * 8) -- bits
      , create "Rate" pliRate
      ]
    ]

clients   = One (Ref "clients")
imageRate = One (Ref "imageRate") -- images / minute
resX      = One (Ref "resX")
resY      = One (Ref "resY")
scale     = One (Ref "scale")
color     = One (Ref "color")
compress  = One (Ref "compress")
pliRate   = One (Ref "pliRate")   -- messages / minute
-- logging   = Ref "logging"


-- ** Configurations

-- | Generate a configuration for CP2. For CP2, we are fixing as constants
--   many properties that may be varied in the future.
networkConfigCP2 :: Int -> Int -> Int -> Double -> [V PVal]
networkConfigCP2 cs pli img scale =
    [ One $ I cs    -- clients
    , One $ I img   -- imageRate
    , One $ I 2500  -- resX
    , One $ I 2000  -- resY
    , chc "color" (B True) (B False) -- color
    , One $ F scale -- scale
    , chc "compress" (B True) (B False) -- compress
    , One $ I pli   -- pliRate
    ]


-- ** Initial environments

-- | Creates an initial resource environment for a given bandwidth (kb/s).
networkEnv :: Double -> ResEnv
networkEnv kbs = envSingle "/Network/Bandwidth" (One . Just $ (F kbs))


-- ** Mission requirements

-- | The only mission requirement is that we don't run out of bandwidth.
networkReqs :: Model
networkReqs = Model [] [check "/Network/Bandwidth" TFloat (val .>= 0)]


--
-- * Driver
--

data NetworkOpts = NetworkOpts
     { genDict   :: Bool
     , genModel  :: Bool
     , genConfig :: Maybe (Int,Int,Int,Double)
     , genEnv    :: Maybe Double
     , genReqs   :: Bool }
  deriving (Data,Eq,Generic,Read,Show,Typeable)

parseNetworkOpts :: Parser NetworkOpts
parseNetworkOpts = NetworkOpts
  <$> switch
       ( long "dict"
      <> help "Generate DFU dictionary" )

  <*> switch
       ( long "model"
      <> help "Generate application model" )

  <*> (optional . option auto)
       ( long "config"
      <> metavar "(clients,pli,img,scale)"
      <> help "Generate configuration with given number of clients, PLI rate (#/min), and image rate (#/min)" )

  <*> (optional . option auto)
       ( long "init"
      <> metavar "bandwidth"
      <> help "Generate initial resource environment with given bandwidth (kb/s)" )

  <*> switch
       ( long "reqs"
      <> help "Generate mission requirements" )


runNetwork :: NetworkOpts -> IO ()
runNetwork opts = do
    when (genDict opts)  (writeJSON defaultDict  networkDFUs)
    when (genModel opts) (writeJSON defaultModel appModel)
    case genConfig opts of
      Just (cs,pli,img,scale) -> writeJSON defaultConfig (networkConfigCP2 cs pli img scale)
      Nothing -> return ()
    case genEnv opts of
      Just b  -> writeJSON defaultInit (networkEnv b)
      Nothing -> return ()
    when (genReqs opts)  (writeJSON defaultReqs  networkReqs)
