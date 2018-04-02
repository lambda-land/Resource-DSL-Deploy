module DSL.Example.CrossApp where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Control.Monad (when)
import Options.Applicative
import Data.Monoid
import Data.Foldable
import qualified Data.Text as T
import qualified Data.Map.Strict as M

import DSL.Types
import DSL.Name
import DSL.Environment
import DSL.Expression
import DSL.Model
import DSL.Primitive
import DSL.Serialize
import DSL.Sugar
{-
compRatio = One (Ref "compRatio")
compRate  = One (Ref "compRate") -- images / minute
encRate   = One (Ref "encRate")
encSec    = One (Ref "encSec")

-- ** Configurations

-- | Generate a configuration for the challenge problem.
crossAppConfig :: Double -> Double -> Double -> Double -> [V PVal]
crossAppConfig cr cr' er es =
    [ One $ F cr
    , One $ F cr'
    , One $ F er
    , One $ F es
    ]

-- ** Initial environments

-- | Creates an initial resource environment for a message of a given size.
crossAppEnv :: Double -> ResEnv
crossAppEnv kbs = envSingle "/CrossApp/Size" (One . Just $ (F kbs))

-- ** Mission requirements

-- | The mission requires us to meet the required throughput threshold and
--   achieve a certain security threshold.
crossAppReqs :: Profile
crossAppReqs = toProfile $ Model []
    [Elems [ check "/CrossApp/Throughput" (One TFloat) (val .>= 0), -- what should these
             check "/CrossApp/Enc" (One TFloat) (val .>= 125)]]     -- values be?

--
-- * Driver
--

data CrossAppOpts = CrossAppOpts
     { genDict   :: Bool
     , genModel  :: Bool
     , genConfig :: Maybe (Double,Double,Double,Double)
     , genEnv    :: Maybe Double
     , genReqs   :: Bool }
  deriving (Data,Eq,Generic,Read,Show,Typeable)

crossAppOpts :: Parser CrossAppOpts
crossAppOpts = CrossAppOpts
  <$> switch
       ( long "dict"
      <> help "Generate DFU dictionary" )

  <*> switch
       ( long "model"
      <> help "Generate application model" )

  <*> (optional . option auto)
       ( long "config"
      <> metavar "(comp ratio,comp rate,enc rate,enc security)"
      <> help "Generate configuration with given compression ratio, compression rate (KiB/s), encryption rate (KiB/s), and encryption security (log2 bits)" )

  <*> (optional . option auto)
       ( long "init"
      <> metavar "bandwidth"
      <> help "Generate initial resource environment with given bandwidth (kb/s)" )

  <*> switch
       ( long "reqs"
      <> help "Generate mission requirements" )


runCrossApp :: CrossAppOpts -> IO ()
runCrossApp opts = do
    when (genDict opts)  (writeJSON defaultDict  crossAppDFUs)
    when (genModel opts) (writeJSON defaultModel appModel)
    case genConfig opts of
      Just (cr,cr',er,es) -> writeJSON defaultConfig (crossAppConfig cr cr' er es)
      Nothing -> return ()
    case genEnv opts of
      Just b  -> writeJSON defaultInit (crossAppEnv b)
      Nothing -> return ()
    when (genReqs opts)  (writeJSON defaultReqs crossAppReqs)
-}
-- ** Initial environment

crossAppResEnv :: ResEnv
crossAppResEnv = envFromListAcc [
    ("/Server/Efficiency",(One . Just . F $ 0)),
    ("/Client/Efficiency",(One . Just . F $ 0))
  ]

-- ** Application model

appModel = Model [Param "serverComp" (One TSymbol), Param "serverEnc" (One TSymbol), Param "clientComp" (One TSymbol), Param "clientEnc" (One TSymbol)]
    [ Elems [
      Load (One . Ref $ "serverComp") [One . Lit . One . B $ True],
      Load (One . Ref $ "serverEnc") [One . Lit . One . B $ True],
      Load (One . Ref $ "clientEnc") [One . Lit . One . B $ False],
      Load (One . Ref $ "clientComp") [One . Lit . One . B $ False],
      create "Efficiency" ((One . Res $ "Server/Efficiency") + (One . Res $ "Client/Efficiency"))
    ]]

-- ** DFUs

cr = One . Res $ "../CompRatio"

data Enc = Enc {
    security :: Double, -- bits of security
    eff      :: Double  -- encoding efficiency s/mb
  } deriving (Eq,Show)

makeEncDFU :: Name -> Enc -> Model
makeEncDFU n e = Model [Param "isServer" (One TBool)]
  [ Elems [
    If (One (Ref "isServer"))
      [Elems [
        create "Security" (One . Lit . One . F . security $ e),
        In "Server" [Elems [
          create "EncAlgorithm" (One . Lit . One . S . Symbol $ n),
          modify "Efficiency" TFloat (val + (One . Lit . One . F . eff $ e) * cr)
        ]]
      ]]
      [Elems [
        In "Client" [Elems [
          create "EncAlgorithm" (One . Lit . One . S . Symbol $ n),
          modify "Efficiency" TFloat (val + (One . Lit . One . F . eff $ e) * cr)
        ]]
      ]]
  ]]

data Comp = Comp {
    compRatio :: Double, -- size of resulting message as a percentage of the original
    compEff   :: Double, -- compression efficiency s/kb
    decompEff :: Double  -- decompression efficiency s/kb
  } deriving (Eq,Show)

makeCompDFU :: Name -> Comp -> Model
makeCompDFU n c = Model [Param "isServer" (One TBool)]
  [ Elems [
    If (One (Ref "isServer"))
      [Elems [
        create "CompRatio" (One . Lit . One . F . compRatio $ c),
        In "Server" [Elems [
          create "CompAlgorithm" (One . Lit . One . S . Symbol $ n),
          modify "Efficiency" TFloat (val + (One . Lit . One . F . compEff $ c))
        ]]
      ]]
      [Elems [
        In "Client" [Elems [
          create "CompAlgorithm" (One . Lit . One . S . Symbol $ n),
          modify "Efficiency" TFloat (val + (One . Lit . One . F . decompEff $ c) * cr)
        ]]
      ]]
  ]]

-- | A dictionary of all the location DFUs with associated names.
crossAppDFUs :: Dictionary
crossAppDFUs = modelDict
    [
      -- Block ciphers
      -- Security data from here: https://en.wikipedia.org/wiki/Cipher_security_summary
      -- Efficiency data from here: https://www.cryptopp.com/benchmarks.html
      encEntry "AES128CTR" (Enc 126.1 (1/4525)),
      encEntry "AES128CBC" (Enc 126.1 (1/1073)),
      encEntry "AES256CTR" (Enc 254.4 (1/3340)),
      encEntry "AES256CBC" (Enc 254.4 (1/805)),
      encEntry "Twofish128CTR" (Enc 128 (1/183)),
      encEntry "Threefish256CTR" (Enc 256 (1/375)),
      encEntry "FastButBroken" (Enc 10 (1/10000)),
      encEntry "SlowButSecure" (Enc 1024 (1/50)),
      encEntry "NoEnc" (Enc 0 0),

      -- Compression algorithms
      -- Data from here: https://quixdb.github.io/squash-benchmark/#results
      compEntry "bzip" (Comp (1/2.05) (1/5.09) (1/12.09)),
      compEntry "gzip" (Comp (1/1.87) (1/20.07) (1/70.05)),
      compEntry "pithy" (Comp (1/1.4) (1/78.09) (1/124.38)),
      compEntry "smallandslow" (Comp (1/5) 1 2),
      compEntry "bigandfast" (Comp (1/1.2) 125 150),
      compEntry "nocomp" (Comp 1 0 0)
    ]
  where
    encEntry n e = (n, makeEncDFU n e)
    compEntry n c = (n, makeCompDFU n c)

-- * Config

crossAppConfig :: [V PVal]
crossAppConfig = [comps, encs, encs, comps]
  where
    encs = foldl' f (One . S . Symbol $ "NoEnc") [
          "AES128CTR"
        , "AES128CBC"
        , "AES256CTR"
        , "AES256CBC"
        , "Twofish128CTR"
        , "Threefish256CTR"
        , "FastButBroken"
        , "SlowButSecure"
      ]
    comps = foldl' f (One . S . Symbol $ "nocomp") [
          "bzip"
        , "gzip"
        , "pithy"
        , "smallandslow"
        , "bigandfast"
      ]
    f r n = Chc (BRef n) (One . S . Symbol $ n) r

--
-- * Driver
--

data CrossAppOpts = CrossAppOpts
     { genDict   :: Bool
     , genModel  :: Bool
     , genInit   :: Bool
     , genConfig :: Bool
--     , genReqs   :: Bool }
     }
  deriving (Data,Eq,Generic,Read,Show,Typeable)

parseCrossAppOpts :: Parser CrossAppOpts
parseCrossAppOpts = CrossAppOpts
  <$> switch
       ( long "dict"
      <> help "Generate DFU dictionary" )

  <*> switch
       ( long "model"
      <> help "Generate application model" )

  <*> switch
       ( long "init"
      <> help "Generate initial resource environment" )

  <*> switch
       ( long "config"
      <> help "Generate default configuration" )
{-
  <*> switch
       ( long "reqs"
      <> help "Generate mission requirements" )
  <*> (optional . strOption)
       ( long "init"
      <> metavar "STRING"
      <> help ("Generate initial resource environment; valid strings: "
               ++ intercalate ", " (fmap (T.unpack . fst) locationEnvs)) )
-}

runCrossApp :: CrossAppOpts -> IO ()
runCrossApp opts = do
    when (genDict opts)   (writeJSON defaultDict crossAppDFUs)
    when (genModel opts)  (writeJSON defaultModel appModel)
    when (genInit opts)   (writeJSON defaultInit crossAppResEnv)
    when (genConfig opts) (writeJSON defaultConfig crossAppConfig)
    --when (genReqs opts)  (writeJSON defaultReqs crossAppReqs)
