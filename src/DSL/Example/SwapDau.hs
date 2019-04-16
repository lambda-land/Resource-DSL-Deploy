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

-- ** Ports and DAUs

-- | A DAU is a discrete component in a larger system. It has an ID, several
--   named ports, and a fixed monetary cost.
--   TODO: For some reason, it also has a fixed "opportunity cost", according
--   to the spec, but that seems wrong to me.
data Dau a = MkDau {
     dauID   :: Name      -- ^ the globally unique ID of this DAU
   , ports   :: [Port a]  -- ^ a mapping from port IDs to values or constraints
   , monCost :: Integer   -- ^ the fixed monetary cost of the DAU
   , oppCost :: Integer   -- ^ the fixed opportunity cost (?) of the DAU
} deriving (Data,Typeable,Generic,Eq,Show)

-- | A port is a named connection to a DAU. Each port may contain several
--   named attributes. For a fully configured DAU, the values of the port
--   attributes will be of type 'PVal', while for an unconfigured DAU, the
--   values of port attributes will be 'Constraint', which capture the
--   range of possible values the port can take on.
data Port a = MkPort {
     portID :: Name
   , attrs  :: Env Name a
} deriving (Data,Typeable,Generic,Eq,Show)

-- | A constraint on a port in a DAU.
data Constraint
   = Exactly PVal
   | OneOf   [PVal]
   | Range   PVal PVal
  deriving (Data,Typeable,Generic,Eq,Show)


-- ** BBN interface

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
   , reqDau  :: Dau Constraint
} deriving (Data,Typeable,Generic,Eq,Show)

-- | An adaptation response consists of configured DAUs.
data Response = MkResponse {
     resDaus :: [ResponseDau]
} deriving (Data,Typeable,Generic,Eq,Show)

-- | A DAU in an adaptation response, which replaces one or more DAUs in the
--   corresponding request, whose ports are configured to single values.
data ResponseDau = MkResponseDau {
     replaces :: [Name]
   , resDau   :: Dau PVal
} deriving (Data,Typeable,Generic,Eq,Show)


--
-- * Find replacement
--

-- | Trivially configure a request into a response.
triviallyConfigure :: Request -> Response
triviallyConfigure (MkRequest ds) = MkResponse (map configDau ds)
  where
    configDau (MkRequestDau _ (MkDau i ps mc oc)) =
        MkResponseDau [i] (MkDau i (map configPort ps) mc oc)
    configPort (MkPort i as) = MkPort i (fmap configAttr as)
    configAttr (Exactly v) = v
    configAttr (OneOf vs)  = head vs
    configAttr (Range v _) = v

-- | Find replacement DAUs in the given dictionary. TODO
findReplacement :: Dictionary -> Request -> Maybe Response
findReplacement _ req = Just (triviallyConfigure req)


--
-- * JSON serialization of BBN interface
--

instance ToJSON a => ToJSON (Dau a) where
  toJSON d = object
    [ "GloballyUniqueId"            .= String (dauID d)
    , "Port"                        .= listValue toJSON (ports d)
    , "BBNDauMonetaryCost"          .= Number (fromInteger (monCost d))
    , "BBNDauOpportunityCost"       .= Number (fromInteger (oppCost d)) ]

asDau :: ParseIt a -> ParseIt (Dau a)
asDau asVal = do
    i <- key "GloballyUniqueId" asText
    ps <- key "Port" (eachInArray (asPort asVal))
    mc <- key "BBNDauMonetaryCost" asIntegral
    oc <- key "BBNDauOpportunityCost" asIntegral
    return (MkDau i ps mc oc)

instance ToJSON a => ToJSON (Port a) where
  toJSON p = object (pid : pattrs)
    where
      pid = "GloballyUniqueId" .= portID p
      pattrs = map entry (toAscList (envAsMap (attrs p)))
      entry (k,v) = k .= toJSON v

asPort :: ParseIt a -> ParseIt (Port a)
asPort asVal = do
    i <- key "GloballyUniqueId" asText
    kvs <- eachInObject asVal 
    return (MkPort i (envFromList (filter notID kvs)))
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
