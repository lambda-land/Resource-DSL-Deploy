module DSL.Example.Network where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Control.Monad (when)
import Options.Applicative

import DSL.Effect
import DSL.Expression
import DSL.Model
import DSL.Name ()
import DSL.Path ()
import DSL.Primitive
import DSL.Resource
import DSL.Serialize
import DSL.Sugar


--
-- * Network Provider Example
--

-- ** Application model

imageParams :: [Param]
imageParams = 
    [ Param "imageRate" TInt
    , Param "resX" TInt
    , Param "resY" TInt
    , Param "scale" TInt  -- 1/scale
    , Param "color" TBool
    , Param "compress" TBool
    ]

saParams :: [Param]
saParams =
    [ Param "pliRate" TInt  -- position location information
    , Param "logging" TBool
    ]

networkDFUs :: Dictionary
networkDFUs = profileDict
    [ ("image-producer", imageProducer)
    , ("image-producer-scale", imageScale)
    , ("image-producer-compress", imageCompress)
    ]

-- | Application model.
appModel :: Model
appModel = Model (Param "clients" TInt : imageParams ++ saParams) []

-- | Base image producer.
imageProducer :: Model
imageProducer = Model imageParams
    [ createUnit "Image"
    , In "Image"
      [ create "Rate"  imageRate
      , create "ResX"  resX
      , create "ResY"  resY
      , create "Color" color
      , create "Size"  $ resX * resY * (color ?? (3,1)) ]
    , If (scale .> 1)
        [ Load (Ref "image-producer-scale") [scale] ]
        []
    , If compress
        [ Load (Ref "image-producer-compress") [] ]
        []
    , modify "/Network/Bandwidth" TInt  -- modifies global network bandwidth
        $ val - imageRate * Res "Image/Size"
    ]

-- | Image scaling DFU.
imageScale :: Model
imageScale = Model [Param "scale" TInt]
    [ modify "CPU" TInt  -- modifies local CPU
        $ val - Res "Image/ResX" * Res "Image/ResY"
    , In "Image"
      [ modify "ResX" TInt $ val ./ scale
      , modify "ResY" TInt $ val ./ scale
      , modify "Size" TInt $ val ./ (scale * scale)
      ]
    ]

-- | Image compression DFU.
imageCompress :: Model
imageCompress = Model []
    [ modify "Image/Size" TInt $ val ./ 2
    , modify "CPU" TInt $ val - Res "Image/ResX" * Res "Image/ResY"
    ]

-- | Situational awareness producer.
saProducer :: Model
saProducer = Model [] []

imageRate = Ref "imageRate"
resX      = Ref "resX"
resY      = Ref "resY"
scale     = Ref "scale"
color     = Ref "color"
compress  = Ref "compress"
pliRate   = Ref "pliRate"
logging   = Ref "logging"


--
-- * Driver
--

data NetworkOpts = NetworkOpts
     { genDict  :: Bool
     , genModel :: Bool
     , genReqs  :: Bool }
  deriving (Data,Eq,Generic,Read,Show,Typeable)

parseNetworkOpts :: Parser NetworkOpts
parseNetworkOpts = NetworkOpts
  <$> switch
       ( long "dict"
      <> help "Generate DFU dictionary" )
  
  <*> switch
       ( long "model"
      <> help "Generate application model" )
  
  <*> switch
       ( long "reqs"
      <> help "Generate mission requirements" )

runNetwork :: NetworkOpts -> IO ()
runNetwork opts = do
    when (genDict opts)  (writeJSON defaultDict networkDFUs)
    -- when (genModel opts) (writeJSON defaultModel undefined)
    -- when (genReqs opts)  (writeJSON defaultReqs undefined)
