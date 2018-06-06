module DSL.Example.CrossApp where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Control.Monad (when)
import Options.Applicative
import Data.Foldable
import Data.Monoid
import Data.Maybe (fromJust)
import Data.SBV ((|||),(&&&),bnot)
import qualified Data.Text as T
import qualified Data.Set as S

import DSL.Types
import DSL.Name
import DSL.Environment
import DSL.Model
import DSL.Serialize
import DSL.Sugar
import DSL.Options


-- ** Initial environment

crossAppResEnv :: CrossAppInit -> ResEnv
crossAppResEnv r = envFromList $ concatMap opt
    [ ("/Server/AES-NI", serverAESNI r)
    , ("/Server/StrongEncryptionPolicy", serverSEP r)
    , ("/Client/AES-NI", clientAESNI r)
    , ("/Client/StrongEncryptionPolicy", clientSEP r) ]
  where
    opt (_,False) = []
    opt (n,True)  = [(n, One (Just Unit))]


-- ** Application model

appModel = Model
  [
    Param "serverProvider" (One TSymbol),
    Param "clientProvider" (One TSymbol)
  ]
  (checkSEP ++ checkRules ++ [Elems [
    In "/Server/Cipher" [Elems [Load (ref "serverProvider") []]],
    In "/Client/Cipher" [Elems [Load (ref "clientProvider") []]],
    check "/Server/Cipher/Algorithm" tSymbol (val .== res "/Client/Cipher/Algorithm"),
    check "/Server/Cipher/Mode"      tSymbol (val .== res "/Client/Cipher/Mode"),
    check "/Server/Cipher/Padding"   tSymbol (val .== res "/Client/Cipher/Padding")
  ]])
  where
    checkSEP :: Block
    checkSEP = [
        Split sepCtx
          [Elems [ checkUnit "/Server/StrongEncryptionPolicy"
                 , checkUnit "/Client/StrongEncryptionPolicy" ]]
          []
      ]

    mkKsz :: [Int] -> [T.Text]
    mkKsz = fmap (\i -> "KSZ" <> (T.pack . show) i)

    allKsz = mkKsz [8,16,24,32,40,48,56,64]

    sepCtx :: BExpr
    sepCtx = fromJust (exclusive allKsz (mkKsz [24,32,40,48,56,64]))

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

    checkRules :: Block
    checkRules = foldMap (uncurry kszRule) algRules

    kszRule :: T.Text -> [Int] -> Block
    kszRule alg is = [
        Split (BRef alg &&& foldOr (disallowed is))
          [Elems [checkUnit "KeySize"]]
          []
      ]

    disallowed is = S.toList $ S.fromList allKsz `S.difference` S.fromList (mkKsz is)

    foldOr [] = BLit False
    foldOr [x] = BRef x
    foldOr (x:xs) = BRef x ||| foldOr xs

-- ** DFUs

mkCrossAppDFU :: T.Text -> [T.Text] -> [T.Text] -> [T.Text] -> Block
mkCrossAppDFU name algs pads modes =
    [ Elems [ create "DFU-Type" (sym "Cipher")
            , create "DFU-Name" (sym name) ]]
        ++ mk "Algorithm" algs ++ mk "Mode" modes ++ mk "Padding" pads
    where
      mk name xs = foldMap (\a -> [Split (foldOthers a xs) [Elems [create name (mkVExpr (S . Symbol $ a)) ]] []]) xs
      others x xs = S.difference (S.fromList xs) (S.singleton x)
      foldOthers x xs = foldl' (\b a -> b &&& (bnot (BRef a))) (BRef x) (S.toList (others x xs))

javaxDFU :: Model
javaxDFU = Model [] $ mkCrossAppDFU "Javax"
             ["AES", "Blowfish", "DES", "DESede", "RC2", "Rijndael"]
             ["PKCS5Padding", "NoPadding"]
             ["ECB", "CBC", "CTR", "CFB", "OFB", "CTS"]

bouncyDFU :: Model
bouncyDFU = Model [] $ mkCrossAppDFU "BouncyCastle"
              ["AES", "ARIA", "Blowfish", "Camellia", "CAST5", "CAST6", "DES", "DESede", "DSTU7624", "GCM", "GOST28147", "IDEA", "Noekeon", "RC2", "RC5", "RC6", "Rijndael", "SEED", "Skipjack", "SM4", "TEA", "Threefish_256", "Threefish_512", "Twofish", "XTEA"]
              ["ZeroBytePadding", "PKCS5Padding", "PKCS7Padding", "ISO10126_2Padding", "ISO7816_4Padding", "TBCPadding", "X923Padding", "NoPadding"]
              ["ECB", "CBC", "CTR", "CFB", "CTS", "OFB", "OpenPGPCFB", "PGPCFBBlock", "SICBlock"]
              ++ [Elems [If (res "Algorithm" .== sym "AES") [Elems [checkUnit "../AES-NI"]] []]]

crossAppDFUs :: Dictionary
crossAppDFUs = modelDict [("Javax", javaxDFU), ("BouncyCastle", bouncyDFU)]

-- * Config

crossAppConfig :: Symbol -> Symbol -> [V PVal]
crossAppConfig s c = [One . S $ s, One . S $ c]

--
-- * Requirements
--

crossAppReqs :: Profile
crossAppReqs = toProfile $ Model [] []

--
-- * Driver
--

data CrossAppConfig = CrossAppConfig
     { serverProv :: String
     , clientProv :: String }
  deriving (Show,Eq,Read,Data,Typeable,Generic)

data CrossAppInit = CrossAppInit
     { serverAESNI :: Bool
     , serverSEP   :: Bool
     , clientAESNI :: Bool
     , clientSEP   :: Bool }
  deriving (Eq,Read,Show,Data,Typeable,Generic)

data CrossAppOpts = CrossAppOpts
     { genDict   :: Bool
     , genModel  :: Bool
     , genInit   :: Maybe CrossAppInit
     , genConfig :: Maybe CrossAppConfig
     , genReqs   :: Bool }
  deriving (Eq,Read,Show,Data,Typeable,Generic)

parseCrossAppOpts :: Parser CrossAppOpts
parseCrossAppOpts = CrossAppOpts
  <$> switch
       ( long "dict"
      <> help "Generate DFU dictionary" )

  <*> switch
       ( long "model"
      <> help "Generate application model" )

  <*> (optional . option (readRecord "CrossAppInit"))
       ( long "init"
      <> metavar "RECORD"
      <> help ("Generate the initial resource environment. The argument RECORD "
         <> "must contain all of the following fields, each of which is "
         <> "followed by either 'True' or 'False': \n"
         <> "  * serverAESNI\n"
         <> "  * clientSEP\n"
         <> "  * clientAESNI\n"
         <> "  * clientSEP") )

  <*> (optional . option (readRecord "CrossAppConfig"))
       ( long "config"
      <> metavar "(serverProv,clientProv)"
      <> help "Generate configuration by giving the name of the provider to use on the server and client" )

  <*> switch
       ( long "reqs"
      <> help "Generate empty mission requirements" )

runCrossApp :: CrossAppOpts -> IO ()
runCrossApp opts = do
    when (genDict opts)   (writeJSON defaultDict crossAppDFUs)
    when (genModel opts)  (writeJSON defaultModel appModel)
    case (genInit opts) of
      Just r  -> writeJSON defaultInit (crossAppResEnv r)
      Nothing -> return ()
    case (genConfig opts) of
      Just (CrossAppConfig s c) -> writeJSON defaultConfig (crossAppConfig (str2sym s) (str2sym c))
      Nothing -> return ()
    when (genReqs opts) (writeJSON defaultReqs crossAppReqs)
    where
      str2sym = Symbol . T.pack
