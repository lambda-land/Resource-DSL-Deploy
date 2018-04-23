module DSL.Example.CrossApp where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Control.Monad (when)
import Options.Applicative
import Data.Monoid
import Data.Foldable
import Data.SBV ((|||),(&&&))
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
    ("/Server/Hardware/AESNI", Chc (dim "SAESNI") (One (Just Unit)) (One Nothing)),
    ("/Client/Hardware/AESNI", Chc (dim "CAESNI") (One (Just Unit)) (One Nothing)),
    ("/Server/JCEUnlimitedStrength", Chc (dim "SJCEUS") (One (Just Unit)) (One Nothing)),
    ("/Client/JCEUnlimitedStrength", Chc (dim "CJCEUS") (One (Just Unit)) (One Nothing))
  ]

-- ** Application model

appModel = Model
  [
    Param "serverProvider" (One TSymbol),
    Param "clientProvider" (One TSymbol),
    Param "keysize" (One TInt)
  ]
  [ Elems [
    In "/Server" [Elems[Load (One . Ref $ "serverProvider") [One . Ref $ "keysize"]]],
    In "/Client" [Elems[Load (One . Ref $ "clientProvider") [One . Ref $ "keysize"]]]
  ]]

-- ** DFUs

javaxDFU :: Model
javaxDFU = Model [Param "keysize" (One TInt)]
  [
    Elems [
      If (ksz .> (Chc (dim "DES") 64 128))
        [Elems[checkUnit "JCEUnlimitedStrength"]]
        [],
      If (Chc (dim "AES")
           aes
           (Chc (dim "Blowfish")
             blowfish
             (Chc (dim "DES")
               des
               (Chc (dim "DES3")
                 des3
                 (mkVExpr . B $ False)
               )
             )
           )
         )
        [Elems[create "KeySize" ksz]]
        [Elems[check "KeySize" (One TInt) (mkVExpr . B $ False)]],
      create "Algorithm" (foldChc ["NOALG", "AES", "Blowfish", "DES", "DES3"]),
      create "Mode" (foldChc ["NOMODE", "ECB", "CBC", "PCBC", "CTR", "CTS", "CFB", "OFB"]),
      create "Padding" (foldChc ["NOPADDING","PKCS5", "ISO10126"])
    ]
  ]
  where
    ksz = One . Ref $ "keysize"
    aes = ksz .== 128 ||| ksz .== 192 ||| ksz .== 256
    blowfish = ksz .>= 32 &&& ksz .<= 448 &&& ksz .% 8 .== 0
    des = ksz .== 56
    des3 = ksz .== 112 &&& ksz .== 168

bcDFU :: Model
bcDFU = Model [Param "keysize" (One TInt)]
  [
    Elems [
      If (Chc (dim "AES")
           aes
           (Chc (dim "Blowfish")
             blowfish
             (Chc (dim "DES")
               des
               (Chc (dim "DES3")
                 des3
                 (Chc (dim "TwoFish")
                   twofish
                   (Chc (dim "ThreeFish")
                     threefish
                     (mkVExpr . B $ False)
                   )
                 )
               )
             )
           )
         )
        [Elems[create "KeySize" ksz]]
        [Elems[check "KeySize" (One TInt) (mkVExpr . B $ False)]],
      create "Algorithm" (foldChc ["NOALG", "AES", "Blowfish", "DES", "DES3", "TwoFish", "ThreeFish"]),
      create "Mode" (foldChc ["NOMODE", "ECB", "CBC", "PCBC", "CTR", "CTS", "CFB", "OFB", "CCM", "EAX"]),
      create "Padding" (foldChc ["NOPADDING","PKCS5", "ISO10126", "X923", "TBC"])
    ]
  ]
  where
    ksz = One . Ref $ "keysize"
    aes = ksz .== 128 ||| ksz .== 192 ||| ksz .== 256
    blowfish = ksz .>= 32 &&& ksz .<= 448 &&& ksz .% 8 .== 0
    des = ksz .== 56
    des3 = ksz .== 112 &&& ksz .== 168
    twofish = aes
    threefish = ksz .== 256 ||| ksz .== 512 ||| ksz .== 1024

aesniDFU :: Model
aesniDFU = Model [Param "keysize" (One TInt)]
  [
    Split (dim "AES")
      [Elems[checkUnit "AESNI"]]
      [],
    Elems [
      If (Chc (dim "AES")
           aes
           (Chc (dim "Blowfish")
             blowfish
             (Chc (dim "DES")
               des
               (Chc (dim "DES3")
                 des3
                 (mkVExpr . B $ False)
               )
             )
           )
         )
        [Elems[create "KeySize" ksz]]
        [Elems[check "KeySize" (One TInt) (mkVExpr . B $ False)]],
      create "Algorithm" (foldChc ["NOALG", "AES", "Blowfish", "DES", "DES3"]),
      create "Mode" (foldChc ["NOMODE", "ECB", "CBC", "PCBC", "CTR", "CTS", "CFB", "OFB"]),
      create "Padding" (foldChc ["NOPADDING","PKCS5", "ISO10126"])
    ]
  ]
  where
    ksz = One . Ref $ "keysize"
    aes = ksz .== 128 ||| ksz .== 192 ||| ksz .== 256
    blowfish = ksz .>= 32 &&& ksz .<= 448 &&& ksz .% 8 .== 0
    des = ksz .== 56
    des3 = ksz .== 112 &&& ksz .== 168

foldChc :: [T.Text] -> V Expr
foldChc [] = error "whoops!"
foldChc (start:names) = One . Lit $ foldl' f (One . S . Symbol $ start) names
  where
    f r n = Chc (BRef n) (One . S . Symbol $ n) r

crossAppDFUs :: Dictionary
crossAppDFUs = modelDict [("Javax", javaxDFU), ("AESNI", aesniDFU), ("BouncyCastle", bcDFU)]

-- * Config

crossAppConfig :: Symbol -> Symbol -> Int -> [V PVal]
crossAppConfig s c ksz = [One . S $ s, One . S $ c, One . I $ ksz]

--
-- * Requirements
--

crossAppReqs :: Symbol -> Int -> Symbol -> Symbol -> Profile
crossAppReqs alg ksz mode pad = toProfile $ Model []
  [ Elems [
      In "Server" chk,
      In "Client" chk
  ]]
    where
      chk = [ Elems [
          check "Algorithm" (One TSymbol) (One (P2 (SS_B SEqu) val (mkVExpr . S $ alg))),
          check "KeySize" (One TInt) (val .== (mkVExpr . I $ ksz)),
          check "Mode" (One TSymbol) (One (P2 (SS_B SEqu) val (mkVExpr . S $ mode))),
          check "Padding" (One TSymbol) (One (P2 (SS_B SEqu) val (mkVExpr . S $ pad)))
        ]]

--
-- * Driver
--

data CrossAppOpts = CrossAppOpts
     { genDict   :: Bool
     , genModel  :: Bool
     , genInit   :: Bool
     , genConfig :: Maybe (String,String,Int)
     , genReqs   :: Maybe (String,Int,String,String) }
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

  <*> (optional . option auto)
       ( long "config"
      <> metavar "(server provider,client provider,keysize)"
      <> help "Generate configuration by giving the name of the provider to use on the server and client, as well as the keysize to use" )

  <*> (optional . option auto)
       ( long "reqs"
      <> metavar "(alg,ksz,mode,pad)"
      <> help "Generate mission requirements for a given algorithm, keysize, mode, and padding scheme (e.g. AES, 256, CTR, and PKCS5)" )

runCrossApp :: CrossAppOpts -> IO ()
runCrossApp opts = do
    when (genDict opts)   (writeJSON defaultDict crossAppDFUs)
    when (genModel opts)  (writeJSON defaultModel appModel)
    when (genInit opts)   (writeJSON defaultInit crossAppResEnv)
    case (genConfig opts) of
      Just (s,c,ksz) -> writeJSON defaultConfig (crossAppConfig (str2sym s) (str2sym c) ksz)
      Nothing -> return ()
    case (genReqs opts) of
      Just (alg,ksz,mode,pad) -> writeJSON defaultReqs (crossAppReqs (str2sym alg) ksz (str2sym mode) (str2sym pad))
      Nothing -> return ()
    where
      str2sym = Symbol . T.pack
