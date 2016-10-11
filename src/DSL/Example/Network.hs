module DSL.Example.Network where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Control.Monad (when)
import Data.List (intercalate,subsequences)
import Options.Applicative

import DSL.Environment
import DSL.Expression
import DSL.Model
import DSL.Primitive
import DSL.Profile
import DSL.Resource
import DSL.Serialize


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
