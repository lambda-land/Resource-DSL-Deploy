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
    
otherParams =
    [ Param "clients" TInt
    , Param "pliRate" TInt
    , Param "imageRate" TInt
    ]

imageParams :: [Param]
imageParams = 
    [ Param "resX" TInt
    , Param "resY" TInt
    , Param "scale" TInt  -- 1/scale
    , Param "color" TBool
    , Param "compress" TBool
    ]

networkDFUs :: Dictionary
networkDFUs = profileDict
    [ ("image-producer", imageProducer)
    , ("image-producer-scale", imageScale)
    , ("image-producer-compress", imageCompress)
    ]

-- | Base image producer.
imageProducer :: Model
imageProducer = Model imageParams
    [ provideUnit "Image"
    , In "Image"
      [ Do "ResX" (Create resX)
      , Do "ResY" (Create resY)
      , Do "Color" (Create color)
      , Do "Size" (Create (resX * resY * color ?? (3,1))) ]
    , If (scale .> 1)
        [ Load (Ref "image-producer-scale") [scale] ]
        []
    , If compress
        [ Load (Ref "image-producer-compress") [] ]
        []
    -- TODO network
    ]

imageScale :: Model
imageScale = Model [Param "scale" TInt]
    [ In "Image"
      [ Do "ResX" (Modify (Fun (Param "x" TInt) (Ref "x" ./ scale)))
      , Do "ResY" (Modify (Fun (Param "y" TInt) (Ref "y" ./ scale)))
      , Do "Size" (Modify (Fun (Param "s" TInt) (Ref "s" ./ (scale * scale))))
      ]
      -- TODO modify network and CPU
    ]

imageCompress :: Model
imageCompress = Model []
    [ In "Image"
      [ Do "Size" (Modify (Fun (Param "s" TInt) (Ref "s" ./ 2))) ]
      -- TODO modify network and CPU
    ]

imageSize :: Expr
imageSize = resX * resY * (color ?? (3,1)) ./ (scale * (compress ?? (2,1)))

cpuCost :: Expr
cpuCost = resX * resY * ((compress ?? (1,0)) + (scale .> 1 ?? (1,0)))

resX     = Ref "resX"
resY     = Ref "resY"
scale    = Ref "scale"
color    = Ref "color"
compress = Ref "compress"


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
