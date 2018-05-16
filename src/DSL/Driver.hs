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
import Data.SBV (bnot,(&&&))

import DSL.Types hiding (Check)
import DSL.Model
import DSL.Name
import DSL.Profile
import DSL.Resource
import DSL.Serialize
import DSL.Parser (parseBExprString)
import DSL.V

import DSL.Example.Location
import DSL.Example.Network
import DSL.Example.CrossApp


--
-- * Run the Program
--

runDriver :: IO ()
runDriver = do
    cmd <- getCommand
    case cmd of
      Check opts -> runCheck opts
      Example (Location opts) -> runLocation opts
      Example (Network opts)  -> runNetwork opts
      Example (CrossApp opts) -> runCrossApp opts
      Verify opts -> runVerify opts

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
getBExpr vars (Total xs) = let
                             xs' = fmap T.pack xs
                             v = S.toList $ vars S.\\ (S.fromList xs')
                           in
                             totalHelper xs' v

totalHelper :: [Var] -> [Var] -> Maybe BExpr
totalHelper [] [] = Nothing
totalHelper [x] ys = Just $ foldr (\a b -> bnot (BRef a) &&& b) (BRef x) ys
totalHelper xs [y] = Just $ foldr (\a b -> BRef a &&& b) (bnot (BRef y)) xs
totalHelper (x:xs) ys = Just $ foldr (\a b -> bnot (BRef a) &&& b)
                              (foldr (\a b -> BRef a &&& b) (BRef x) xs)
                               ys

getSel :: Select a => SelOpts -> (a -> a)
getSel (Formula s) = case (parseBExprString s) of
                       Right b -> sel b
                       Left e -> do
                         error $ "Could not parse selection:\n" ++ e
getSel (OnOff ns fs) =
  case (ns,fs) of
    (Just (x:xs), Just (y:ys)) -> sel ((f (x:xs)) &&& (g (y:ys)))
    (Just (x:xs), _) -> sel $ f (x:xs)
    (_, Just (y:ys)) -> sel $ g (y:ys)
    _ -> id
  where
    f [] = error "impossible"
    f [x] = BRef (T.pack x)
    f (x:xs) = BRef (T.pack x) &&& f xs
    g [] = error "impossible"
    g [x] = bnot (BRef (T.pack x))
    g (x:xs) = bnot (BRef (T.pack x)) &&& g xs
getSel (Total []) = conf (BLit False)
getSel (Total xs) = conf $ f xs
  where
    f [] = error "impossible"
    f [x] = BRef (T.pack x)
    f (x:xs) = (BRef (T.pack x)) &&& f xs

runCheck :: CheckOpts -> IO ()
runCheck opts = do
    let s = getSel (selection opts)
    dict' <- readJSON (dictFile opts) asDictionary
    let dict = s dict'
    init' <- readJSON (initFile opts) asResEnv
    let init = s init'
    model' <- readJSON (modelFile opts) asModel
    let model = s model'
    args' <- case configValue opts of
              Just xs -> decodeJSON xs asConfig
              Nothing -> readJSON (configFile opts) asConfig
    let args = map (One . Lit . s) args'
    let vars = getVars dict' <> getVars init' <> getVars model' <> foldMap getVars args'
    let b = getBExpr vars (selection opts)
    sctx <- runWithDict dict init (loadModel model args) `catchEffErr` (opts, 2,"Error executing application model ...", b)
    writeOutput (outputFile opts) sctx
    if noReqs opts then do
      writeError (errorFile opts) sctx
      writeSuccess (successFile opts) sctx b
    else do
      reqs' <- readJSON (reqsFile opts) asProfile
      let reqs = s reqs'
      let (e, sctx') = runWithDict' dict sctx (loadProfile reqs [])
      writeError (errorFile opts) sctx'
      writeSuccess (successFile opts) sctx' (getBExpr (vars <> getVars reqs') (selection opts))
      case e of
        (Left _) -> putStrLn "Requirements not satisfied ..." >> exitWith (ExitFailure 3)
        (Right _) -> return ()
    putStrLn "Success"

catchEffErr :: (Either a b, StateCtx) -> (CheckOpts, Int, String, Maybe BExpr) -> IO StateCtx
catchEffErr (Left _, s) (opts, code,msg,b) = do
  writeError (errorFile opts) s
  writeSuccess (successFile opts) s b
  putStrLn (msg)
  exitWith (ExitFailure code)
catchEffErr (Right _, s) _ = return s

writeError :: FilePath -> StateCtx -> IO ()
writeError fp (SCtx _ _ e) = writeJSON fp e

writeSuccess :: FilePath -> StateCtx -> Maybe BExpr -> IO ()
writeSuccess fp (SCtx _ s _) Nothing = writeJSON fp (bnot s)
writeSuccess fp (SCtx _ s _) (Just b) = writeJSON fp (bnot s &&& b)

writeOutput :: FilePath -> StateCtx -> IO ()
writeOutput fp (SCtx o _ _) = writeJSON fp o

runVerify :: Verify -> IO ()
runVerify = _

--
-- * Command Line Arguments
--

data Command
     = Check CheckOpts
     | Example Example
     | Verify Verify
  deriving (Data,Eq,Generic,Read,Show,Typeable)

data SelOpts = Formula String
             | OnOff {on :: Maybe [String], off :: Maybe [String]}
             | Total [String]
  deriving (Data,Eq,Generic,Read,Show,Typeable)

data CheckOpts = CheckOpts
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

data Verify = VerifyOpts { verOpts :: SelOpts }
  deriving (Data,Eq,Generic,Read,Show,Typeable)


getCommand :: IO Command
getCommand = getArgs >>= handleParseResult . execParserPure pref desc
  where
    pref = prefs (columns 100)
    desc = info (helper <*> parseCommand) fullDesc

parseCommand :: Parser Command
parseCommand = subparser
     ( command "run"
        (info (Check <$> (helper <*> parseCheckOpts))
        (progDesc ("Execute an application model on a given resource environment; "
          ++ "optionally run result against given mission requirements")))
    <> command "check"
        (info (Verify <$> (helper <*> parseVerify))
        (progDesc "Check a configuration against the latest run call"))
    <> command "example"
        (info (Example <$> (helper <*> parseExample))
        (progDesc "Generate example inputs and put them in the inbox")) )

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

parseCheckOpts :: Parser CheckOpts
parseCheckOpts = CheckOpts
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
       ( long "success-context"
       <> value defaultCtx
       <> help "Global success context")
  where
    pathOption mods = strOption (mods <> showDefault <> metavar "FILE")

parseVerify :: Parser Verify
parseVerify = VerifyOpts <$> parseSel
