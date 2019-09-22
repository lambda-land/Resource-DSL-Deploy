{-# LANGUAGE NoMonomorphismRestriction #-}

module DSL.Driver where

import Prelude hiding (init)

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Data.List ((\\))
import Data.Monoid ((<>))
import Data.Set (Set,empty,toList)
import Data.Text (pack)
import Options.Applicative hiding (empty)
import System.Environment (getArgs)
import System.Exit

import DSL.Boolean
import DSL.Evaluation
import DSL.Parser (parseBExprString)
import DSL.SAT
import DSL.Serialize
import DSL.Types hiding (Check)
import DSL.Preparation

import DSL.Example.CrossApp
import DSL.Example.Location
import DSL.Example.Network
import DSL.Example.SwapDau


--
-- * Run the Program
--

-- | Top level driver.
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

-- | Compute a selection from the set of all features and the selection options.
getSelection :: Set Var -> Maybe SelOpts -> BExpr
getSelection _  Nothing   = true
getSelection vs (Just os) = case os of
    Formula s -> case parseBExprString s of
      Right sel -> sel
      Left err  -> error ("Could not parse selection: " ++ err)
    OnOff ons offs -> gen (map pack ons) (map pack offs)
    Total ons ->
      let ons' = map pack ons in gen ons' (toList vs \\ ons')
  where
    gen ons offs = foldr (&&&) true $ map BRef ons ++ map (bnot . BRef) offs

-- | Execute the 'run' command.
run :: RunOpts -> IO ()
run opts = do
    
    -- read in all the inputs
    dict  <- readJSON (dictFile opts) asDictionary
    init  <- readJSON (initFile opts) asResEnv
    model <- readJSON (modelFile opts) asModel
    reqs  <- readJSON (reqsFile opts) asModel
    args  <- fmap (map (One . Lit)) $ case configValue opts of
      Just xs -> decodeJSON xs asConfig
      Nothing -> readJSON (configFile opts) asConfig
    
    -- compute the selection and update the inputs
    -- TODO: deal with integer dimensions
    let dims = boolDims dict
            <> boolDims init
            <> boolDims model
            <> boolDims reqs
            <> boolDims args
    let sel = getSelection dims (selection opts)
    let (dict', init', model', reqs', args') = case selection opts of
          Nothing -> (dict, init, model, reqs, args)
          Just (Total _) ->
            (configure sel dict, configure sel init, configure sel model,
             configure sel reqs, configure sel args)
          _ ->
            (select sel dict, select sel init, select sel model,
             select sel reqs, select sel args)
    
    -- run the model and check against requirements
    let Model [] reqsBlock = reqs'
    let Model xs mainBlock = model'
    let toRun = Model xs (mainBlock ++ if noReqs opts then [] else reqsBlock)
    (_, sctx) <- runEvalWith dict' init' dims empty (loadModel toRun args')
    
    -- write the outputs
    let passed = bnot (errCtx sctx)
    writeJSON (outputFile opts) (resEnv sctx)
    writeJSON (errorFile opts) (vError sctx)
    writeJSON (successFile opts) (SuccessCtx passed dims)
    putStrLn "Done. Run 'check' to determine if any configurations were successful."

-- | Execute the 'check' command.
runCheck :: CheckOpts -> IO ()
runCheck opts = do
    succ <- readJSON (successF opts) asSuccess
    let sel = getSelection (configSpace succ) (verOpts opts)
    let passed = sel &&& successCtx succ
    res <- satResults (maxRes opts) passed
    writeFile (bestFile opts) (show res)
    if take 2 (show res) == "No" then do
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

data SelOpts
   = Formula String
   | OnOff [String] [String]
   | Total [String]
  deriving (Data,Eq,Generic,Read,Show,Typeable)

data RunOpts = RunOpts
     { noReqs      :: Bool
     , configValue :: Maybe String
     , selection   :: Maybe SelOpts
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
      verOpts  :: Maybe SelOpts
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

parseSel :: Parser (Maybe SelOpts)
parseSel = optional (formula <|> onOff <|> total)
  where
    formula = Formula
      <$> strOption
          ( short 'f'
          <> long "formula"
          <> metavar "STRING"
          <> help "A boolean expression indicating the variants to be executed." )
    onOff = OnOff
      <$> option auto
          ( long "on"
          <> metavar "STRING-LIST"
          <> help "A list of features to be enabled." )
      <*> option auto
          ( long "off"
          <> metavar "STRING-LIST"
          <> help "A list of features to be disabled." )
    total = Total
      <$> option auto
          ( long "total"
          <> short 't'
          <> metavar "STRING-LIST"
          <> help "A list of feature to be enabled, all others will be disabled." )

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
