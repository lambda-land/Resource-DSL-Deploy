module DSL.Example.SwapDau where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Control.Monad (when)
import Data.Aeson
import Data.Aeson.BetterErrors
import Data.Aeson.Types (listValue)
import Data.Map.Strict (toAscList)
import Options.Applicative hiding ((<|>))
import System.Exit

import DSL.Environment
import DSL.Model
import DSL.Name
import DSL.Serialize
import DSL.Types


--
-- * Types
--

-- ** Ports

-- | A list of ports.
type Ports a = [Port a]

-- | A list of port groups.
type PortGroups a = [PortGroup a]

-- | Named attributes associated with a port. For a fully configured DAU,
--   the values of the port attributes will be of type 'PVal', while for an
--   unconfigured DAU, the values of port attributes will be 'Constraint',
--   which captures the range of possible values the port can take on.
type PortAttrs a = Env Name a

-- | A port is a named connection to a DAU and its associated attributes.
data Port a = MkPort {
     portID    :: Name
   , portFunc  :: Name
   , portAttrs :: PortAttrs a
} deriving (Data,Typeable,Generic,Eq,Show)

-- | A port group is a set of ports with identical attributes.
data PortGroup a = MkPortGroup {
     groupIDs   :: [Name]      -- ^ list of port IDs in this group
   , groupAttrs :: Env Name a  -- ^ attributes shared among ports in the group
   , groupSize  :: Int         -- ^ number of ports in the group
}

-- | A constraint on a port in an unconfigured DAU.
data Constraint
   = Exactly PVal
   | OneOf   [PVal]
   | Range   PVal PVal
  deriving (Data,Typeable,Generic,Eq,Show)


-- ** DAUs

-- | A DAU is a discrete component in a larger system. It has an ID, several
--   named ports, and a fixed monetary cost. The ports of a DAU may be either
--   grouped or ungrouped; the type parameter 'p' is used to range over these
--   two possibilities.
data Dau p = MkDau {
     dauID   :: Name      -- ^ globally unique ID of this DAU
   , ports   :: p         -- ^ named ports
   , monCost :: Integer   -- ^ fixed monetary cost of the DAU
} deriving (Data,Typeable,Generic,Eq,Show)


-- ** Requests and Responses

-- | An adaptation request contains the relevant DAUs that may need to be
--   replaced.
data Request = MkRequest {
     reqDaus :: [RequestDau]
} deriving (Data,Typeable,Generic,Eq,Show)

-- | A DAU in an adaptation request, which may be flagged for replacement,
--   whose ports are constrained according to the requirements of the
--   system it lives in.
data RequestDau = MkRequestDau {
     replace :: Bool
   , reqDau  :: Dau (Ports Constraint)
} deriving (Data,Typeable,Generic,Eq,Show)

-- | An adaptation response consists of configured DAUs.
data Response = MkResponse {
     resDaus :: [ResponseDau]
} deriving (Data,Typeable,Generic,Eq,Show)

-- | A DAU in an adaptation response, which replaces one or more DAUs in the
--   corresponding request, whose ports are configured to single values.
data ResponseDau = MkResponseDau {
     replaces :: [Name]
   , resDau   :: Dau (Ports PVal)
} deriving (Data,Typeable,Generic,Eq,Show)


--
-- * Find replacement
--

-- | Trivially configure a request into a response.
triviallyConfigure :: Request -> Response
triviallyConfigure (MkRequest ds) = MkResponse (map configDau ds)
  where
    configDau (MkRequestDau _ (MkDau i ps mc)) =
        MkResponseDau [i] (MkDau i (map configPort ps) mc)
    configPort (MkPort i fn as) = MkPort i fn (fmap configAttr as)
    configAttr (Exactly v) = v
    configAttr (OneOf vs)  = head vs
    configAttr (Range v _) = v

-- | Find replacement DAUs in the given dictionary. TODO
findReplacement :: Dictionary -> Request -> Maybe Response
findReplacement _ req = Just (triviallyConfigure req)


--
-- * JSON serialization of BBN interface
--

instance ToJSON a => ToJSON (Dau (Ports a)) where
  toJSON d = object
    [ "GloballyUniqueId"            .= String (dauID d)
    , "Port"                        .= listValue toJSON (ports d)
    , "BBNDauMonetaryCost"          .= Number (fromInteger (monCost d)) ]

asDau :: ParseIt a -> ParseIt (Dau (Ports a))
asDau asVal = do
    i <- key "GloballyUniqueId" asText
    ps <- key "Port" (eachInArray (asPort asVal))
    mc <- key "BBNDauMonetaryCost" asIntegral
    return (MkDau i ps mc)

instance ToJSON a => ToJSON (Port a) where
  toJSON p = object (pid : pfn : pattrs)
    where
      pid = "GloballyUniqueId" .= portID p
      pfn = "BBNPortFunctionality" .= portFunc p
      pattrs = map entry (toAscList (envAsMap (portAttrs p)))
      entry (k,v) = k .= toJSON v

asPort :: ParseIt a -> ParseIt (Port a)
asPort asVal = do
    i <- key "GloballyUniqueId" asText
    fn <- key "BBNPortFunctionality" asText
    kvs <- eachInObject asVal 
    return (MkPort i fn (envFromList (filter notID kvs)))
  where
    notID (k,_) = k /= "GloballyUniqueId"

instance ToJSON Constraint where
  toJSON (Exactly v)   = toJSON v
  toJSON (OneOf vs)    = listValue toJSON vs
  toJSON (Range lo hi) = object [ "Min" .= toJSON lo, "Max" .= toJSON hi ]

asConstraint :: ParseIt Constraint
asConstraint = Exactly <$> asPVal
    <|> OneOf <$> eachInArray asPVal
    <|> Range <$> key "Min" asPVal <*> key "Max" asPVal

instance ToJSON Request where
  toJSON r = object
    [ "daus" .= listValue toJSON (reqDaus r) ]

asRequest :: ParseIt Request
asRequest = MkRequest <$> key "daus" (eachInArray asRequestDau)

instance ToJSON RequestDau where
  toJSON r = case toJSON (reqDau r) of
      Object o -> Object (o <> attr)
      _ -> error "RequestDau#toJSON: internal error"
    where
      attr = "BBNDauFlaggedForReplacement" .= Bool (replace r)

asRequestDau :: ParseIt RequestDau
asRequestDau = do
    r <- key "BBNDauFlaggedForReplacement" asBool
    d <- asDau asConstraint
    return (MkRequestDau r d)

instance ToJSON Response where
  toJSON r = object
    [ "daus" .= listValue toJSON (resDaus r) ]

asResponse :: ParseIt Response
asResponse = MkResponse <$> key "daus" (eachInArray asResponseDau)

instance ToJSON ResponseDau where
  toJSON r = case toJSON (resDau r) of
      Object o -> Object (o <> attr)
      _ -> error "RequestDau#toJSON: internal error"
    where
      attr = "SupersededDauIds" .= listValue String (replaces r)

asResponseDau :: ParseIt ResponseDau
asResponseDau = do
    rs <- key "replaces" (eachInArray asText)
    d <- asDau asPVal
    return (MkResponseDau rs d)


--
-- * Driver options
--

defaultDictFile, defaultRequestFile, defaultResponseFile :: FilePath
defaultDictFile     = "inbox/swap-dictionary.json"
defaultRequestFile  = "inbox/swap-request.json"
defaultResponseFile = "outbox/swap-response.json"

data SwapOpts = MkSwapOpts {
     swapRunSearch    :: Bool
   , swapDictFile     :: FilePath
   , swapRequestFile  :: FilePath
   , swapResponseFile :: FilePath
} deriving (Data,Typeable,Generic,Eq,Read,Show)

parseSwapOpts :: Parser SwapOpts
parseSwapOpts = MkSwapOpts

    <$> switch
         ( long "run"
        <> help "Run the search for replacement DAUs" )

    <*> pathOption
         ( long "dict-file"
        <> value defaultDictFile
        <> help "Path to the JSON DAU dictionary file" )

    <*> pathOption
         ( long "request-file"
        <> value defaultRequestFile
        <> help "Path to the JSON request file" )

    <*> pathOption
         ( long "response-file"
        <> value defaultResponseFile
        <> help "Path to the JSON response file" )
  where
    pathOption mods = strOption (mods <> showDefault <> metavar "FILE")

runSwap :: SwapOpts -> IO ()
runSwap opts = do
    when (swapRunSearch opts) $ do
      dict <- readJSON (swapDictFile opts) asDictionary -- TODO use DAU format
      req  <- readJSON (swapRequestFile opts) asRequest
      putStrLn "Searching for replacement DAUs ..."
      case findReplacement dict req of
        Just res -> do 
          let resFile = swapResponseFile opts
          writeJSON resFile res
          putStrLn ("Success. Response written to: " ++ resFile)
        Nothing -> do
          putStrLn "No replacement DAUs found."
          exitWith (ExitFailure 3)
