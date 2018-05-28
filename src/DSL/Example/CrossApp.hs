module DSL.Example.CrossApp where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Control.Monad (when)
import Options.Applicative
import Data.Foldable
import Data.Monoid
import Data.SBV ((|||),(&&&),bnot)
import qualified Data.Text as T
import qualified Data.Set as S

import DSL.Types
import DSL.Name
import DSL.Environment
import DSL.Expression ((??))
import DSL.Model
import DSL.Serialize
import DSL.Sugar
import DSL.Options

-- ** Initial environment

crossAppResEnv :: ResEnv
crossAppResEnv = envEmpty

-- ** Application model

appModel = Model
  [
    Param "serverProvider" (One TSymbol),
    Param "clientProvider" (One TSymbol)
  ]
  [ Elems [
    In "/Server" [Elems[Load (One . Ref $ "serverProvider") []]],
    In "/Client" [Elems[Load (One . Ref $ "clientProvider") []]]
  ]]

-- ** DFUs

mkCrossAppDFU :: [T.Text] -> [T.Text] -> [T.Text] -> Model
mkCrossAppDFU algs pads modes = Model []
  (mk "Algorithm" algs ++ mk "Mode" modes ++ mk "Padding" pads)
    where
      mk name xs = foldMap (\a -> [Split (foldOthers a xs) [Elems[ create name (mkVExpr (S . Symbol $ a)) ]] []]) xs
      others x xs = S.difference (S.fromList xs) (S.singleton x)
      foldOthers x xs = foldl' (\b a -> b &&& (bnot (BRef a))) (BRef x) (S.toList (others x xs))

javaxDFU :: Model
javaxDFU = mkCrossAppDFU
             ["AES", "Blowfish", "DES", "DESede", "RC2", "Rijndael"]
             ["PKCS5Padding", "NoPadding"]
             ["ECB", "CBC", "CTR", "CFB", "OFB", "CTS"]

bouncyDFU :: Model
bouncyDFU = mkCrossAppDFU
              ["AES", "ARIA", "Blowfish", "Camellia", "CAST5", "CAST6", "DES", "DESede", "DSTU7624", "GCM", "GOST28147", "IDEA", "Noekeon", "RC2", "RC5", "RC6", "Rijndael", "SEED", "Skipjack", "SM4", "TEA", "Threefish_256", "Threefish_512", "Twofish", "XTEA"]
              ["ZeroBytePadding", "PKCS5Padding", "PKCS7Padding", "ISO10126_2Padding", "ISO7816_4Padding", "TBCPadding", "X923Padding", "NoPadding"]
              ["ECB", "CBC", "CTR", "CFB", "CTS", "OFB", "OpenPGPCFB", "PGPCFBBlock", "SICBlock"]

crossAppDFUs :: Dictionary
crossAppDFUs = modelDict [("Javax", javaxDFU), ("BouncyCastle", bouncyDFU)]

-- * Config

crossAppConfig :: Symbol -> Symbol -> [V PVal]
crossAppConfig s c = [One . S $ s, One . S $ c]

--
-- * Requirements
--

crossAppReqs :: Symbol -> Int -> Symbol -> Symbol -> Profile
crossAppReqs alg ksz mode pad = toProfile $ Model []
  (kszRule alg ++ [ Elems [
      In "Server" chk,
      In "Client" chk
  ]])
    where
      chk = [ Elems [
          check "Algorithm" (One TSymbol) (One (P2 (SS_B SEqu) val (mkVExpr . S $ alg))),
          check "Mode" (One TSymbol) (One (P2 (SS_B SEqu) val (mkVExpr . S $ mode))),
          check "Padding" (One TSymbol) (One (P2 (SS_B SEqu) val (mkVExpr . S $ pad)))
        ]]
      algRules = [
          ("AES", [16, 24, 32]),
          ("ARIA", [16, 24, 32]),
          ("Blowfish", [8, 16, 24, 32, 40, 48, 56, 64]),
          ("Camellia", [16, 24, 32]),
          ("CAST5", [8, 16]),
          ("CAST6", [8, 16, 24, 32, 40, 48, 56, 64]),
          ("DES", [8]),
          ("DESede", [24, 16]),
          ("DSTU7624", [16, 32]),
          ("GCM", [16, 24, 32]),
          ("GOST28147", [32]),
          ("IDEA", [8, 16, 24, 32, 40, 48, 56, 64]),
          ("Noekeon", [16, 24, 32, 40, 48, 56, 64]),
          ("RC2", [8, 16, 24, 32, 40, 48, 56, 64]),
          ("RC5", [8, 16, 24, 32, 40, 48, 56, 64]),
          ("RC5_64", []),
          ("RC6", [8, 16, 24, 32, 40, 48, 56, 64]),
          ("Rijndael", [16, 24, 32]),
          ("SEED", [16, 24, 32, 40, 48, 56, 64]),
          ("SEEDWrap", []),
          ("Serpent_128", []),
          ("Skipjack", [16, 24, 32, 40, 48, 56, 64]),
          ("SM4", [16]),
          ("TEA", [16]),
          ("Threefish_256", [32]),
          ("Threefish_512", [64]),
          ("Threefish_1024", []),
          ("Twofish", [8, 16, 24, 32]),
          ("XTEA", [16])
        ]
      kszRule name = case lookup name algRules of
        Just kszs -> [Elems[check "Server/Algorithm" (One TSymbol) (One ((f ksz kszs) ?? (mkVExpr (B True),mkVExpr (B False))))]]
        Nothing -> []

      f :: Int -> [Int] -> V Expr
      f _ [] = mkVExpr (B False)
      f ksz [i] = mkVExpr (I ksz) .== mkVExpr (I i)
      f ksz (i:is) = (mkVExpr (I ksz) .== mkVExpr (I i)) ||| f ksz is


--
-- * Driver
--

data CAConfig = CAConfig {
    serverProv :: String,
    clientProv :: String
  } deriving (Show,Eq,Read,Data,Typeable,Generic)

data CAReqs = CAReqs {
    algorithm :: String,
    keysize :: Int,
    mode :: String,
    padding :: String
  } deriving (Show,Eq,Read,Data,Typeable,Generic)

data CrossAppOpts = CrossAppOpts
     { genDict   :: Bool
     , genModel  :: Bool
     , genInit   :: Bool
     , genConfig :: Maybe CAConfig
     , genReqs   :: Maybe CAReqs}
  deriving (Eq,Read,Show,Data,Typeable,Generic)

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

  <*> (optional . option (readRecord "CAConfig"))
       ( long "config"
      <> metavar "(serverProv,clientProv)"
      <> help "Generate configuration by giving the name of the provider to use on the server and client" )

  <*> (optional . option (readRecord "CAReqs"))
       ( long "reqs"
      <> metavar "(algorithm,keysize,mode,padding)"
      <> help "Generate mission requirements for a given algorithm, keysize in bytes, mode, and padding scheme (e.g. AES, 24, CTR, and PKCS5Padding)" )

runCrossApp :: CrossAppOpts -> IO ()
runCrossApp opts = do
    when (genDict opts)   (writeJSON defaultDict crossAppDFUs)
    when (genModel opts)  (writeJSON defaultModel appModel)
    when (genInit opts)   (writeJSON defaultInit crossAppResEnv)
    case (genConfig opts) of
      Just (CAConfig s c) -> writeJSON defaultConfig (crossAppConfig (str2sym s) (str2sym c))
      Nothing -> return ()
    case (genReqs opts) of
      Just (CAReqs alg ksz mode pad) -> writeJSON defaultReqs (crossAppReqs (str2sym alg) ksz (str2sym mode) (str2sym pad))
      Nothing -> return ()
    where
      str2sym = Symbol . T.pack
