module DSL.Example.CrossApp where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Control.Monad (when)
import Options.Applicative
import Data.Monoid
import Data.Foldable
import qualified Data.Text as T

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

appModel :: Model
appModel = Model
  [
    Param "sCompName" (One TSymbol),
    Param "sCompEff" (One TFloat),
    Param "sCompRatio" (One TFloat),
    Param "sEncName" (One TSymbol),
    Param "sEncEff" (One TFloat),
    Param "sEncSec" (One TFloat),
    Param "cEncName" (One TSymbol),
    Param "cEncEff" (One TFloat),
    Param "cEncSec" (One TFloat),
    Param "cCompName" (One TSymbol),
    Param "cCompEff" (One TFloat),
    Param "cCompRatio" (One TFloat)
  ]
  [Elems[
    Load (One . Lit . One . S . Symbol $ "compression")
      [
        One . Ref $ "sCompName",
        One . Ref $ "sCompEff",
        One . Ref $ "sCompRatio",
        One . Lit . One . B $ True
      ],
    Load (One . Lit . One . S . Symbol $ "encryption")
      [
        One . Ref $ "sEncName",
        One . Ref $ "sEncEff",
        One . Ref $ "sEncSec",
        One . Lit . One . B $ True
      ],
    Load (One . Lit . One . S . Symbol $ "encryption")
      [
        One . Ref $ "cEncName",
        One . Ref $ "cEncEff",
        One . Ref $ "cEncSec",
        One . Lit . One . B $ False
      ],
    Load (One . Lit . One . S . Symbol $ "compression")
      [
        One . Ref $ "cCompName",
        One . Ref $ "cCompEff",
        One . Ref $ "cCompRatio",
        One . Lit . One . B $ False
      ],
      create "/Efficiency" ((One . Res $ "/Server/Efficiency") + (One . Res $ "/Client/Efficiency"))
  ]]

-- ** DFUs

cr :: V Expr
cr = One . Res $ "../CompRatio"

encDFU :: Model
encDFU = Model
  [
    Param "name" (One TSymbol),
    Param "eff" (One TFloat),
    Param "sec" (One TFloat),
    Param "isServer" (One TBool)
  ]
  [Elems[
    If (One . Ref $ "isServer")
      [Elems[
        create "/Security" (One . Ref $ "sec"),
        In "/Server" both
      ]]
      [Elems[
        In "/Client" both
      ]]
  ]]
  where
    both = [Elems[
        create "EncAlgorithm" (One . Ref $ "name"),
        modify "Efficiency" TFloat (val + (One . Ref $ "eff") * cr)
      ]]

compDFU :: Model
compDFU = Model
  [
    Param "name" (One TSymbol),
    Param "eff" (One TFloat),
    Param "cr" (One TFloat),
    Param "isServer" (One TBool)
  ]
  [Elems[
    If (One . Ref $ "isServer")
      [Elems[
        create "/CompRatio" (One . Ref $ "cr"),
        In "/Server"
          [Elems[
            create "CompAlgorithm" (One . Ref $ "name"),
            modify "Efficiency" TFloat (val + (One . Ref $ "eff"))
          ]]
      ]]
      [Elems[
        In "/Client"
          [Elems[
            create "CompAlgorithm" (One . Ref $ "name"),
            modify "Efficiency" TFloat (val + (One . Ref $ "eff") * cr)
          ]]
      ]]
  ]]

crossAppDFUs :: Dictionary
crossAppDFUs = modelDict [
    ("encryption", encDFU),
    ("compression", compDFU)
  ]

-- ** Config

data Enc = Enc {
    ename :: T.Text,
    sec   :: Double,
    eff   :: Double
  } deriving (Eq,Show)

-- Block ciphers
-- Security data from here: https://en.wikipedia.org/wiki/Cipher_security_summary
-- Efficiency data from here: https://www.cryptopp.com/benchmarks.html
encs :: [Enc]
encs =
  [
    Enc "AES128CTR" 126.1 (1/4525),
    Enc "AES128CBC" 126.1 (1/1073),
    Enc "AES256CTR" 254.4 (1/3340),
    Enc "AES256CBC" 254.4 (1/805),
    Enc "Twofish128CTR" 128 (1/183),
    Enc "Threefish256CTR" 256 (1/375),
    Enc "FastButBroken" 10 (1/10000),
    Enc "SlowButSecure" 1024 (1/50)
  ]

data Comp = Comp {
    cname :: T.Text,
    compr :: Double,
    ceff  :: Double,
    deff  :: Double
  } deriving (Eq,Show)

-- Compression algorithms
-- Data from here: https://quixdb.github.io/squash-benchmark/#results
comps :: [Comp]
comps =
  [
    Comp "bzip" (1/2.05) (1/5.09) (1/12.09),
    Comp "gzip" (1/1.87) (1/20.07) (1/70.05),
    Comp "pithy" (1/1.4) (1/78.09) (1/124.38),
    Comp "smallandslow" (1/5) 1 2,
    Comp "bigandfast" (1/1.2) (1/125) (1/150)
  ]

crossAppConfig :: [V PVal]
crossAppConfig = cs ++ es ++ es ++ ds
  where
    cs = [cnames, ceffs, crs]
    ds = [cnames, deffs, crs]
    es = [enames, eeffs, esecs]
    (cnames,crs,ceffs,deffs) = foldl' fc (fstc (Comp "nocomp" 1 0 0)) comps
    (enames,eeffs,esecs) = foldl' fe (fste (Enc "NoEnc" 0 0)) encs
    fstc (Comp n cr ce de) = (One . S . Symbol $ n, One . F $ cr, One . F $ ce, One . F $ de)
    fste (Enc n s e) = (One . S . Symbol $ n, One . F $ s, One . F $ e)
    fc (rn, rcr, rce, rde) (Comp n cr ce de) = let
        f l r = Chc (BRef n) (One l) r
      in
        (f (S . Symbol $ n) rn, f (F cr) rcr, f (F ce) rce, f (F de) rde)
    fe (rn, rs, re) (Enc n s e) = let
        f l r = Chc (BRef n) (One l) r
      in
        (f (S . Symbol $ n) rn, f (F s) rs, f (F e) re)

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
