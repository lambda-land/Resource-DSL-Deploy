module DSL.Example.CrossApp where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Control.Monad (when)
import Options.Applicative
import Data.Monoid
import Data.Foldable

import DSL.Types
import DSL.Name
import DSL.Environment
import DSL.Model
import DSL.Serialize
import DSL.Sugar

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
-- * Requirements
--

crossAppReqs :: Double -> Double -> Double -> Profile
crossAppReqs sec size eff = toProfile $ Model []
  [Elems[
    check "/Server/CompAlgorithm" (One TSymbol) (One (P2 (SS_B SEqu) val (One . Res $ "/Client/CompAlgorithm"))),
    check "/Server/EncAlgorithm" (One TSymbol) (One (P2 (SS_B SEqu) val (One . Res $ "/Client/EncAlgorithm"))),
    check "/Security" (One TFloat) (val .>= (One . Lit . One . F $ sec)),
    check "/CompRatio" (One TFloat) (val .<= (One . Lit . One . F $ size)),
    check "/Efficiency" (One TFloat) (val .<= (One . Lit . One . F $ eff))
  ]]

--
-- * Driver
--

data CrossAppOpts = CrossAppOpts
     { genDict   :: Bool
     , genModel  :: Bool
     , genInit   :: Bool
     , genConfig :: Bool
     , genReqs   :: Maybe (Double,Double,Double) }
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
  <*> (optional . option auto)
       ( long "reqs"
      <> metavar "(security,size,efficiency)"
      <> help "Generate mission requirements based on desired security, message size, and efficiency (s/mb)" )

runCrossApp :: CrossAppOpts -> IO ()
runCrossApp opts = do
    when (genDict opts)   (writeJSON defaultDict crossAppDFUs)
    when (genModel opts)  (writeJSON defaultModel appModel)
    when (genInit opts)   (writeJSON defaultInit crossAppResEnv)
    when (genConfig opts) (writeJSON defaultConfig crossAppConfig)
    case (genReqs opts) of
      Just (sec,size,eff) -> writeJSON defaultReqs (crossAppReqs sec size eff)
      Nothing -> return ()
