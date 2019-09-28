module DSL.Example.SwapDau.Driver where

import Data.Data (Typeable)
import GHC.Generics (Generic)

import Control.Monad (when)
import Options.Applicative hiding ((<|>))
import System.Exit
import Z3.Base (getVersion)

import DSL.Serialize
import DSL.Types

import DSL.Example.SwapDau.Search
import DSL.Example.SwapDau.Serialize
import DSL.Example.SwapDau.Types


--
-- * Driver
--

-- | Top-level driver.
runSwap :: SwapOpts -> IO ()
runSwap opts = do
    when (swapZ3Version opts) $ do
      v <- getVersion
      putStrLn ("z3 version: " ++ show v)
    when (swapRunSearch opts) $ do
      req <- readJSON (swapRequestFile opts) asRequest
      MkSetInventory daus <- readJSON (swapInventoryFile opts) asSetInventory
      rules <- readJSON (swapRulesFile opts) asRules
      putStrLn "Searching for replacement DAUs ..."
      (metrics, result) <- findReplacement (swapMaxDaus opts) rules (createInventory daus) req
      case result of
        Just res -> do 
          let resFile = swapResponseFile opts
          writeJSON resFile res
          writeJSON (swapMetricsFile opts) metrics
          putStrLn ("Success. Response written to: " ++ resFile)
        Nothing -> do
          writeJSON (swapMetricsFile opts) metrics
          putStrLn "No replacement DAUs found."
          exitWith (ExitFailure 3)

-- | A simplified driver suitable for testing.
runSwapTest :: SwapOpts -> IO (Maybe Response)
runSwapTest opts = do
    req <- readJSON (swapRequestFile opts) asRequest
    MkSetInventory daus <- readJSON (swapInventoryFile opts) asSetInventory
    rules <- readJSON (swapRulesFile opts) asRules
    fmap snd $ findReplacement (swapMaxDaus opts) rules (createInventory daus) req

-- | Trivially configure a request into a response (for testing).
triviallyConfigure :: Request -> Response
triviallyConfigure (MkRequest ds) = MkResponse (map configDau ds)
  where
    configDau (MkRequestDau _ (MkDau i ps mc)) =
        MkResponseDau [i] (MkDau i (map configPort ps) mc)
    configPort (MkPort i fn as) = MkResponsePort i (MkPort i fn (fmap configAttr as))
    configAttr (Exactly v)  = Leaf v
    configAttr (OneOf vs)   = Leaf (head vs)
    configAttr (Range v _)  = Leaf (I v)
    configAttr (Sync ss)    = Node (fmap configAttr (head ss))


-- ** Command-line options

defaultMaxDaus :: Int
defaultMaxDaus   = 2

defaultRulesFile, defaultInventoryFile, defaultRequestFile, defaultResponseFile :: FilePath
defaultRulesFile     = "inbox/swap-rules.json"
defaultInventoryFile = "inbox/swap-inventory.json"
defaultRequestFile   = "inbox/swap-request.json"
defaultResponseFile  = "outbox/swap-response.json"
defaultMetricsFile   = "outbox/swap-metrics.json"

data SwapOpts = MkSwapOpts {
     swapRunSearch     :: Bool
   , swapZ3Version     :: Bool
   , swapMaxDaus       :: Int
   , swapRulesFile     :: FilePath
   , swapInventoryFile :: FilePath
   , swapRequestFile   :: FilePath
   , swapResponseFile  :: FilePath
   , swapMetricsFile   :: FilePath
} deriving (Typeable,Generic,Eq,Read,Show)

defaultOpts :: SwapOpts
defaultOpts = MkSwapOpts
    True False 2
    defaultRulesFile
    defaultInventoryFile
    defaultRequestFile
    defaultResponseFile
    defaultMetricsFile

parseSwapOpts :: Parser SwapOpts
parseSwapOpts = MkSwapOpts

    <$> switch
         ( long "run"
        <> help "Run the search for replacement DAUs" )

    <*> switch
         ( long "z3-version"
        <> help "Print the z3 version number" )

    <*> intOption 
         ( long "max-daus"
        <> value defaultMaxDaus
        <> help "Max number of DAUs to include in response; 0 for no limit" )

    <*> pathOption
         ( long "rules-file"
        <> value defaultRulesFile
        <> help "Path to the JSON rules file" )

    <*> pathOption
         ( long "inventory-file"
        <> value defaultInventoryFile
        <> help "Path to the JSON DAU inventory file" )

    <*> pathOption
         ( long "request-file"
        <> value defaultRequestFile
        <> help "Path to the JSON request file" )

    <*> pathOption
         ( long "response-file"
        <> value defaultResponseFile
        <> help "Path to the JSON response file" )

    <*> pathOption
         ( long "metrics-file"
        <> value defaultMetricsFile
        <> help "Path to the JSON metrics file" )
  where
    intOption mods = option auto (mods <> showDefault <> metavar "INT")
    pathOption mods = strOption (mods <> showDefault <> metavar "FILE")
