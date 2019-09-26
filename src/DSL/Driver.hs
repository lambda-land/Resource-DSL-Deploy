{-# LANGUAGE NoMonomorphismRestriction #-}

module DSL.Driver where

import Prelude hiding (init)

import Data.Data (Typeable)
import GHC.Generics (Generic)

import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (pack)
import Options.Applicative hiding (empty)
import System.Environment (getArgs)
import System.Exit
import Z3.Monad (modelToString)

import DSL.Condition
import DSL.Environment
import DSL.Evaluation
import DSL.SAT
import DSL.Serialize
import DSL.Types hiding (Check)
import DSL.Variational

import DSL.Example.CrossApp
import DSL.Example.Location
import DSL.Example.Network
import DSL.Example.SwapDau


--
-- * Run the program
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

-- | Construct a set of variables from a list of strings.
varSet :: [String] -> Set Var
varSet = Set.fromList . map pack

-- | Create a total configuration from the set of all configuration options
--   and a set to turn on.
totalConfig :: Set Var -> Set Var -> Env Var Bool
totalConfig all ons = Set.foldr go envEmpty all
  where go x = envExtend x (Set.member x ons)

-- | Create a partial configuration from a set of configuration options to
--   turn on and a set to turn off.
partialConfig :: Set Var -> Set Var -> Env Var Bool
partialConfig ons offs = envUnion (sub True ons) (sub False offs)
  where sub b = Set.foldr (\x -> envExtend x b) envEmpty

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
    let (dictS, initS, modelS, reqsS, argsS) = case selection opts of
          Nothing -> (dict, init, model, reqs, args)
          Just (Total ons) ->
            let cfg = totalConfig dims (varSet ons)
                shrink = configure cfg envEmpty
            in (shrink dict, shrink init, shrink model,
                shrink reqs, shrink args)
          Just (OnOff ons offs) ->
            let cfg = partialConfig (varSet ons) (varSet offs)
                shrink = reduce cfg envEmpty
            in (shrink dict, shrink init, shrink model,
                shrink reqs, shrink args)

    -- initialize the SAT solver and prepare everything for execution
    z3 <- initSolver
    syms <- symEnvFresh z3 dims Set.empty
    dictP  <- runSat z3 (prepare syms dictS)
    initP  <- runSat z3 (prepare syms initS)
    modelP <- runSat z3 (prepare syms modelS)
    reqsP  <- runSat z3 (prepare syms reqsS)
    argsP  <- runSat z3 (prepare syms argsS)
    
    -- run the model and check against requirements
    let Model [] reqsBlock = reqsP
    let Model xs mainBlock = modelP
    let toRun = Model xs (mainBlock ++ if noReqs opts then [] else reqsBlock)
    (_, sctx) <- runEval z3 dictP initP (loadModel toRun argsP)
    
    -- write the outputs
    passed <- runSat z3 (condNot (errCtx sctx))
    writeJSON (outputFile opts) (resEnv sctx)
    writeJSON (errorFile opts) (vError sctx)
    writeJSON (successFile opts) (SuccessCtx (condExpr passed) dims)
    putStrLn "Done. Run 'check' to determine if any configurations were successful."

-- | Execute the 'check' command.
--   TODO: This currently does not take either the selection or the max-results
--   into account!
runCheck :: CheckOpts -> IO ()
runCheck opts = do
    succ <- readJSON (successF opts) asSuccess
    z3 <- initSolver
    res <- runSat z3 (symBExprFresh (successCtx succ) >>= satModel)
    case res of
      Nothing -> do
        putStrLn "No successful configurations found."
        exitWith (ExitFailure 4)
      Just sol -> do
        solStr <- runSat z3 (modelToString sol)
        writeFile (bestFile opts) (show solStr)
        putStrLn "Success"

--
-- * Command Line Arguments
--

data Command
     = Run RunOpts
     | Example Example
     | Check CheckOpts
     | Swap SwapOpts
  deriving (Eq,Generic,Read,Show,Typeable)

data SelOpts
   = OnOff [String] [String]
   | Total [String]
  deriving (Eq,Generic,Read,Show,Typeable)

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
  deriving (Eq,Generic,Read,Show,Typeable)

data Example
     = Location LocationOpts
     | Network  NetworkOpts
     | CrossApp CrossAppOpts
  deriving (Eq,Generic,Read,Show,Typeable)

data CheckOpts = CheckOpts {
      verOpts  :: Maybe SelOpts
    , successF :: FilePath
    , bestFile :: FilePath
    , maxRes :: Int
  } deriving (Eq,Generic,Read,Show,Typeable)


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
parseSel = optional (onOff <|> total)
  where
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
