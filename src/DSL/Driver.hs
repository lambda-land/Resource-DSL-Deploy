{-# LANGUAGE NoMonomorphismRestriction #-}

module DSL.Driver where

import Prelude hiding (init)

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Data.Monoid ((<>))
import Options.Applicative
import System.Environment (getArgs)
import System.Exit
import qualified Data.Text as T
import qualified Data.Set as S
import Data.SBV (AllSatResult(..))

import DSL.Boolean
import DSL.Model
import DSL.Name
import DSL.Parser (parseBExprString)
import DSL.Profile
import DSL.Resource
import DSL.SAT
import DSL.Serialize
import DSL.Types hiding (Check)
import DSL.V

import DSL.Example.CrossApp
import DSL.Example.Location
import DSL.Example.Network
import DSL.Example.SwapDau


--
-- * Run the Program
--

runDriver :: IO ()
runDriver = do
    cmd <- getCommand
    case cmd of
      Run opts -> run opts
      Example (CrossApp opts) -> runCrossApp opts
      Example (Location opts) -> runLocation opts
      Example (Network opts)  -> runNetwork opts
      Swap opts  -> runSwap opts
      Check opts -> runCheck opts

getBExpr :: S.Set Var -> SelOpts -> Maybe BExpr
getBExpr _ (Formula s) = case (parseBExprString s) of
                            Right b -> Just b
                            Left e -> do
                              error $ "Could not parse selection:\n" ++ e
getBExpr _ (OnOff ns fs) =
  case (ns,fs) of
    (Just (x:xs), Just (y:ys)) -> Just ((f (x:xs)) &&& (g (y:ys)))
    (Just (x:xs), _) -> Just $ f (x:xs)
    (_, Just (y:ys)) -> Just $ g (y:ys)
    _ -> Nothing
  where
    f [] = error "impossible"
    f [x] = BRef (T.pack x)
    f (x:xs) = BRef (T.pack x) &&& f xs
    g [] = error "impossible"
    g [x] = bnot (BRef (T.pack x))
    g (x:xs) = bnot (BRef (T.pack x)) &&& g xs
getBExpr vars (Total xs) | null vars = Nothing
                         | otherwise = let
                             xs' = fmap T.pack xs
                             s = S.fromList xs' `S.intersection` vars
                             xs'' = S.toList s
                             vars' = S.toList $ vars S.\\ s
                           in
                             totalHelper xs'' vars'

totalHelper :: [Var] -> [Var] -> Maybe BExpr
totalHelper [] [] = Nothing
totalHelper [] (y:ys) = Just $ foldr (\a b -> (bnot (BRef a)) &&& b) (bnot $ BRef y) ys
totalHelper [x] ys = Just $ foldr (\a b -> (bnot (BRef a)) &&& b) (BRef x) ys
totalHelper xs [y] = Just $ foldr (\a b -> BRef a &&& b) (bnot (BRef y)) xs
totalHelper (x:xs) ys = Just $ foldr (\a b -> (bnot (BRef a)) &&& b)
                              (foldr (\a b -> BRef a &&& b) (BRef x) xs)
                               ys

getSel :: Variational a => SelOpts -> a -> (S.Set Var, a)
getSel opts a = (vars, a')
  where
    vars = dimensions a
    b = getBExpr vars opts
    a' = case b of
           Just b' -> case opts of
             Total _ -> configure b' a
             _       -> select b' a
           Nothing -> a

run :: RunOpts -> IO ()
run opts = do
    let s x = x >>= (\y -> return $ getSel (selection opts) y)
    (dictVars, dict) <- s $ readJSON (dictFile opts) asDictionary
    (initVars, init) <- s $ readJSON (initFile opts) asResEnv
    (modelVars, model) <- s $ readJSON (modelFile opts) asModel
    (argsVars, args') <- s $ case configValue opts of
              Just xs -> decodeJSON xs asConfig
              Nothing -> readJSON (configFile opts) asConfig
    let args = map (One . Lit) args'
    let vars = dictVars <> initVars <> modelVars <> argsVars
    let b = getBExpr vars (selection opts)
    sctx <- runWithDict dict init (loadModel model args) `catchEffErr` (opts, 2,"Error executing application model ...", b, vars)
    writeOutput (outputFile opts) sctx
    if noReqs opts then do
      writeError (errorFile opts) sctx
      writeSuccess (successFile opts) sctx b vars
    else do
      (reqsVars,reqs) <- s $ readJSON (reqsFile opts) asProfile
      let (e, sctx') = runWithDict' dict sctx (loadProfile reqs [])
      writeError (errorFile opts) sctx'
      let vars' = vars <> reqsVars
      writeSuccess (successFile opts) sctx' (getBExpr vars' (selection opts)) vars'
      case e of
        (Left _) -> putStrLn "Requirements not satisfied ..." >> exitWith (ExitFailure 3)
        (Right _) -> return ()
    putStrLn "Success"

catchEffErr :: (Either a b, StateCtx) -> (RunOpts, Int, String, Maybe BExpr, S.Set Var) -> IO StateCtx
catchEffErr (Left _, s) (opts, code,msg,b,vs) = do
  writeError (errorFile opts) s
  writeSuccess (successFile opts) s b vs
  putStrLn (msg)
  exitWith (ExitFailure code)
catchEffErr (Right _, s) _ = return s

writeError :: FilePath -> StateCtx -> IO ()
writeError fp (SCtx _ _ e) = writeJSON fp e

writeSuccess :: FilePath -> StateCtx -> Maybe BExpr -> S.Set Var -> IO ()
writeSuccess fp (SCtx _ s _) Nothing vs = writeJSON fp (SuccessCtx (bnot s) vs)
writeSuccess fp (SCtx _ s _) (Just b) vs = writeJSON fp (SuccessCtx (bnot s &&& b) vs)

writeOutput :: FilePath -> StateCtx -> IO ()
writeOutput fp (SCtx o _ _) = writeJSON fp o

writeBest :: FilePath -> AllSatResult -> IO ()
writeBest fp r = writeFile fp (show r)

runCheck :: CheckOpts -> IO ()
runCheck opts = do
    s <- readJSON (successF opts) asSuccess
    let b = case getBExpr (cfgSpc s) (verOpts opts) of
              Just b -> b &&& (ctx s)
              Nothing -> ctx s
    r <- satResults (maxRes opts) b
    writeBest (bestFile opts) r
    if take 2 (show r) == "No" then do
      putStrLn "No successful configurations found."
      exitWith (ExitFailure 4)
    else
      putStrLn "Success"

--
-- * Command Line Arguments
--

data Command
     = Run RunOpts
     | Example Example
     | Check CheckOpts
     | Swap SwapOpts
  deriving (Data,Eq,Generic,Read,Show,Typeable)

data SelOpts = Formula String
             | OnOff {on :: Maybe [String], off :: Maybe [String]}
             | Total [String]
  deriving (Data,Eq,Generic,Read,Show,Typeable)

data RunOpts = RunOpts
     { noReqs      :: Bool
     , configValue :: Maybe String
     , selection   :: SelOpts
     , dictFile    :: FilePath
     , initFile    :: FilePath
     , modelFile   :: FilePath
     , configFile  :: FilePath
     , reqsFile    :: FilePath
     , outputFile  :: FilePath
     , errorFile   :: FilePath
     , successFile :: FilePath }
  deriving (Data,Eq,Generic,Read,Show,Typeable)

data Example
     = Location LocationOpts
     | Network  NetworkOpts
     | CrossApp CrossAppOpts
  deriving (Data,Eq,Generic,Read,Show,Typeable)

data CheckOpts = CheckOpts {
      verOpts :: SelOpts
    , successF :: FilePath
    , bestFile :: FilePath
    , maxRes :: Int
  } deriving (Data,Eq,Generic,Read,Show,Typeable)


getCommand :: IO Command
getCommand = getArgs >>= handleParseResult . execParserPure pref desc
  where
    pref = prefs (columns 100)
    desc = info (helper <*> parseCommand) fullDesc

parseCommand :: Parser Command
parseCommand = subparser
     ( command "run"
        (info (Run <$> (helper <*> parseRunOpts))
        (progDesc ("Execute an application model on a given resource environment; "
          ++ "optionally run result against given mission requirements")))
    <> command "check"
        (info (Check <$> (helper <*> parseCheck))
        (progDesc "Check a configuration against the latest run call"))
    <> command "example"
        (info (Example <$> (helper <*> parseExample))
        (progDesc "Generate example inputs and put them in the inbox"))
    <> command "swap-dau"
        (info (Swap <$> (helper <*> parseSwapOpts))
        (progDesc "Find replacement DAUs"))
     )

parseExample :: Parser Example
parseExample = subparser
     ( command "location"
        (info (Location <$> (helper <*> parseLocationOpts))
        (progDesc "Location provider example"))
    <> command "network"
        (info (Network <$> (helper <*> parseNetworkOpts))
        (progDesc "Network / image provider example"))
    <> command "crossapp"
        (info (CrossApp <$> (helper <*> parseCrossAppOpts))
        (progDesc "Cross application dependencies example"))
     )

parseSel :: Parser SelOpts
parseSel = formula <|> onOff <|> total
  where
    formula = Formula
      <$> strOption
          ( short 'f'
          <> long "formula"
          <> metavar "STRING"
          <> help "A string representing a boolean expression. Selects variants to be executed." )
    onOff = OnOff
      <$> (optional . option auto)
          ( long "on"
          <> help ("Provide a list of strings representing the names of features" <>
          " that will be set to True and selected on.") )
      <*> (optional . option auto)
          ( long "off"
          <> help ("Provide a list of strings representing the names of features" <>
          " that will be set to False and selected on.") )
    total = Total
      <$> option auto
          ( long "total"
          <> short 't'
          <> help ("Provide a list of strings representing the names of features" <>
          " that will be set to True and selected on. All other features will be" <>
          " set to False.") )

parseRunOpts :: Parser RunOpts
parseRunOpts = RunOpts
  <$> switch
       ( short 'n'
      <> long "no-reqs"
      <> help "Don't check output environment against mission requirements" )

  <*> (optional . strOption)
       ( short 'c'
      <> long "config"
      <> metavar "STRING"
      <> help "Arguments to the application model; overrides --config-file if present" )

  <*> parseSel

  <*> pathOption
       ( long "dict-file"
      <> value defaultDict
      <> help "Dictionary of DFU profiles" )

  <*> pathOption
       ( long "init-file"
      <> value defaultInit
      <> help "Initial resource environment" )

  <*> pathOption
       ( long "model-file"
      <> value defaultModel
      <> help "Application model" )

  <*> pathOption
       ( long "config-file"
      <> value defaultConfig
      <> help "Arguments to the application model" )

  <*> pathOption
       ( long "reqs-file"
      <> value defaultReqs
      <> help "Mission requirements profile" )

  <*> pathOption
       ( long "output-file"
      <> value defaultOutput
      <> help "Final resource environment" )
  <*> pathOption
       ( long "error-file"
       <> value defaultError
       <> help "Global error value")
  <*> pathOption
       ( long "success-file"
       <> value defaultCtx
       <> help "Global success context")
  where
    pathOption mods = strOption (mods <> showDefault <> metavar "FILE")

parseCheck :: Parser CheckOpts
parseCheck = CheckOpts
  <$> parseSel
  <*> pathOption
       ( long "success-file"
       <> value defaultCtx
       <> help "Global success context")
  <*> pathOption
       ( long "best-file"
       <> value defaultBest
       <> help "List of possible successful configurations")
  <*> option auto
       ( long "max-results"
       <> short 'm'
       <> value 25
       <> showDefault
       <> metavar "INTEGER"
       <> help "Maximum successful configurations shown")
  where
    pathOption mods = strOption (mods <> showDefault <> metavar "FILE")
