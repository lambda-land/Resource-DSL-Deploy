{-# OPTIONS_GHC -Wno-type-defaults #-}

module DSL.Example.SwapDau where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Control.Monad (when)
import Data.Aeson
import Data.Aeson.BetterErrors
import Data.Aeson.Types (listValue)
import Data.Function (on)
import Data.List (nubBy,sortBy,subsequences)
import qualified Data.Set as Set
import Data.Text (pack)
import Data.SBV (Boolean(..))
import Data.String (fromString)
import Options.Applicative hiding ((<|>))
import System.Exit

import DSL.Environment
import DSL.Model
import DSL.Name
import DSL.Primitive
import DSL.Resource
import DSL.SAT
import DSL.Serialize
import DSL.Sugar
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
     portID    :: Name          -- ^ unique ID
   , portFunc  :: Name          -- ^ core functionality of this port
   , portAttrs :: PortAttrs a   -- ^ named port attributes
} deriving (Data,Typeable,Generic,Eq,Show)

-- | A port group is a set of ports with identical attributes.
data PortGroup a = MkPortGroup {
     groupIDs   :: [Name]       -- ^ list of port IDs in this group
   , groupFunc  :: Name         -- ^ functionality of the ports in this group
   , groupAttrs :: PortAttrs a  -- ^ attributes shared among ports in the group
   , groupSize  :: Int          -- ^ number of ports in the group
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

-- | A DAU inventory is a list of available DAUs, sorted by monetary cost,
--   where the ports in each DAU have been organized into groups.
type Inventory = [Dau (PortGroups Constraint)]


-- ** Requests and Responses

-- | Set the current DAU inventory.
data SetInventory = MkSetInventory {
     invDaus :: [Dau (Ports Constraint)]
} deriving (Data,Typeable,Generic,Eq,Show)

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
-- * Port grouping
--

-- | Group ports with identical attributes together within a DAU.
groupPortsInDau :: Eq a => Dau (Ports a) -> Dau (PortGroups a)
groupPortsInDau (MkDau n ps c) = MkDau n (groupPorts ps) c

-- | Group ports with identical attributes together.
groupPorts :: Eq a => Ports a -> PortGroups a
groupPorts ps = do
    MkPort _ fn as <- unique
    let qs = filter (\p -> as == portAttrs p) ps
    return (MkPortGroup (map portID qs) fn as (length qs))
  where
    unique = nubBy same ps
    same p1 p2 = portFunc p1 == portFunc p2 && portAttrs p1 == portAttrs p2


--
-- * Inventories
--

-- | Monetary cost of a (sub-)inventory.
inventoryCost :: Inventory -> Integer
inventoryCost = sum . map monCost

-- | Convert a list of ungrouped DAUs into a DAU inventory.
createInventory :: [Dau (Ports Constraint)] -> Inventory
createInventory ds = sortBy (compare `on` monCost) (map groupPortsInDau ds)

-- | Filter inventory to include only DAUs that provide functionalities
--   relevant to the given port groups.
filterInventory :: PortGroups a -> Inventory -> Inventory
filterInventory gs ds = filter (any relevant . map groupFunc . ports) ds
  where
    fns = Set.fromList (map groupFunc gs)
    relevant fn = Set.member fn fns

-- | Sort a list of inventories by monetary cost. Note that this forces
--   computing all subsequences of the inventory up front and so will
--   only scale to dictionaries of 20 or so relevant DAUs. If we instead
--   apply some basic heuristics (e.g. limit to 2-3 replacement DAUs), we
--   can lazily explore this space and so scale better.
sortInventories :: [Inventory] -> [Inventory]
sortInventories = sortBy (compare `on` inventoryCost)

-- | All subsequences of a list up to a maximum length.
subsUpToLength :: Int -> [a] -> [[a]]
subsUpToLength k xs = [] : subs k xs
  where
    subs 0 _      = []
    subs _ []     = []
    subs k (x:xs) = map (x:) (subsUpToLength (k-1) xs) ++ subs k xs


--
-- * DSL encoding
--

-- ** Dimension names

-- | Generate an infinite sequence of dimensions from the given prefix. Used
--   for choosing among several possibilities when configuring port attributes.
dimN :: Var -> [Var]
dimN pre = [mconcat [pre, "+", pack (show i)] | i <- [1..]]

-- | Dimension indicating how to configure a port attribute.
dimAttr :: Name -> Int -> Name -> Var
dimAttr dau grp att = mconcat [dau, "+", pack (show grp), "+", att]

-- | Dimension indicating whether a provided DAU + group is used to (partially)
--   satisfy a required DAU + port group.
dimUseGroup :: Name -> Int -> Name -> Int -> Var
dimUseGroup provDau provGrp reqDau reqGrp =
    mconcat [provDau, "+", pack (show provGrp), "+", reqDau, "+", pack (show reqGrp)]


-- ** Provisions

-- | Translate a DAU inventory into a dictionary.
toDictionary :: Inventory -> Dictionary
toDictionary = envFromList . map entry
  where
    entry d = (mkSymbol (dauID d), ModEntry (provideDau d))

-- | Encode a provided DAU as a DSL model.
provideDau :: Dau (PortGroups Constraint) -> Model
provideDau (MkDau n gs c) = Model []
    [ Elems [
        modify "/MonetaryCost" TInt (val + fromInteger c)
      , In (Path Relative [n])
        [ Elems (zipWith (providePortGroup n) gs [1..]) ]
    ]]

-- | Encode a provided port group as a DSL statement.
providePortGroup :: Name -> PortGroup Constraint -> Int -> Stmt
providePortGroup dau g i =
    In (fromString ("Group/" ++ show i))
    [ Elems [
      create "Functionality" (sym (groupFunc g))
    , create "PortCount" (lit (I (groupSize g)))
    , In "Attributes"
      [ Elems $ do
        ((n,c),i) <- zip (envToList (groupAttrs g)) [1..]
        return (providePortAttr (dimAttr dau i n) n c)
      ]
    ]]

-- | Encode a provided port attribute as a DSL statement.
providePortAttr :: Var -> Name -> Constraint -> Stmt
providePortAttr dim att c = Do (Path Relative [att]) (effect c)
  where
    effect (Exactly v)   = Create (lit v)
    effect (OneOf vs)    = Create (One (Lit (chcN (dimN dim) vs)))
    effect (Range lo hi) = Create (One (Lit (chc dim lo hi)))
      -- TODO only allows configuring for one of the extremes...


-- ** Requirements

-- | Check all required DAUs against the inventory of provisions.
requireDaus :: Inventory -> [Dau (PortGroups Constraint)] -> [Stmt]
requireDaus inv reqs = concatMap (requireDau inv) reqs

-- | Check a required DAU against the inventory of provisions.
requireDau :: Inventory -> Dau (PortGroups Constraint) -> [Stmt]
requireDau inv req = do
    (g,i) <- zip (ports req) [1..]
    requirePortGroup inv (dauID req) i g
        
-- | Check whether the required port group is satisfied by the inventory
--   of provisions; adjust the ports-to-match and available ports accordingly.
requirePortGroup
  :: Inventory             -- ^ provided DAU inventory
  -> Name                  -- ^ required DAU name
  -> Int                   -- ^ index of required group
  -> PortGroup Constraint  -- ^ required group
  -> [Stmt]
requirePortGroup inv reqDauName reqGrpIx reqGrp =
    -- keep track of the ports we still have to match for this group
    (create "/PortsToMatch" (lit (I (groupSize reqGrp))) : do
       provDau <- inv
       let provDauName = dauID provDau
       provGrpIx <- [1 .. length (ports provDau)]
       let dim = dimUseGroup provDauName provGrpIx reqDauName reqGrpIx
       return $
         In (Path Relative [provDauName, "Group", pack (show provGrpIx)])
         [ Elems [checkGroup dim reqGrp] ])
    ++ [check "/PortsToMatch" tInt (val .== 0)]
        
-- | Check a required port group against the port group in the current context.
checkGroup :: Var -> PortGroup Constraint -> Stmt
checkGroup dim grp =
    -- if there are ports left to match, ports left in this group,
    -- and this group provides the right functionality
    If (res "/PortsToMatch" .> 0
         &&& res "PortCount" .> 0
         &&& res "Functionality" .==. sym (groupFunc grp))
    [ Split (BRef dim)  -- tracks if this group was used for this requirement
      [ Elems [
        -- check all of the attributes
        In "Attributes" [ Elems $ do
          (n,c) <- envToList (groupAttrs grp)
          return (requirePortAttr n c)
        ]
        -- if success, update the ports available and required
      , if' (res "/PortsToMatch" .> res "PortCount") [
          modify "PortCount" TInt 0
        , modify "/PortsToMatch" TInt (val - res "PortCount")
        ] [
          modify "PortCount" TInt (val - res "/PortsToMatch")
        , modify "/PortsToMatch" TInt 0
        ]
      ]]
      []
     ]
     -- else branch
     [ Split (BRef dim)
       -- force dim to be false to track that we didn't use this group
       [ Elems [ checkUnit "/Fail" ] ]
       []
     ]

-- | Check whether the required attribute is satisfied by the current port group.
requirePortAttr :: Name -> Constraint -> Stmt
requirePortAttr att c = check (Path Relative [att]) (One (ptype c)) (body c)
  where
    ptype (Exactly v)  = primType v
    ptype (OneOf vs)   = primType (head vs)
    ptype (Range lo _) = primType lo
    body (Exactly v)   = val .== lit v
    body (OneOf vs)    = foldr1 (|||) [val .== lit v | v <- vs]
    body (Range lo hi) = val .== lit lo ||| val .== lit hi


-- ** Application model

-- | Generate the application model for a given inventory and set of DAUs to
--   replace.
appModel :: Inventory -> [Dau (PortGroups Constraint)] -> Model
appModel inv daus = Model []
    [ Elems $
       create "/MonetaryCost" 0
    :  [Load (sym (dauID d)) [] | d <- inv]
    ++ requireDaus inv daus
    ]

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

-- | Extract the list of DAUs to replace from a request and group their ports.
toReplace :: Request -> [Dau (PortGroups Constraint)]
toReplace = map (groupPortsInDau . reqDau) . filter replace . reqDaus

-- | Generate sub-inventories to search, filtered based on the given list of
--   DAUs to replace and an integer indicating the maximum size of each
--   sub-inventory. A max size less than 1 indicates unbounded sub-inventory
--   size (which will be slow for large inventories).
toSearch :: Int -> [Dau (PortGroups Constraint)] -> Inventory -> [Inventory]
toSearch size daus inv = sortInventories $
    (if size > 0 then subsUpToLength size else subsequences) filtered
  where
    filtered = filterInventory (concatMap ports daus) inv

-- | Find replacement DAUs in the given inventory.
findReplacement :: Int -> Inventory -> Request -> IO (Maybe Response)
findReplacement mx inv req = do
    -- debugging
    writeJSON "outbox/swap-dictionary-debug.json" dict
    writeJSON "outbox/swap-model-debug.json" (appModel (invs !! 1) daus)
    putStrLn (show (test (invs !! 1)))
    -- do search
    case loop invs of
      Nothing -> return Nothing
      Just ctx -> do 
        r <- satResults 1 ctx
        writeFile "outbox/swap-solution.txt" (show r)
        return Nothing
  where
    dict = toDictionary inv
    daus = toReplace req
    invs = toSearch mx daus inv
    test i = runWithDict dict envEmpty (loadModel (appModel i daus) [])
    loop []     = Nothing
    loop (i:is) = case test i of
        (Left _, _) -> loop is
        (Right _, SCtx _ ctx _) -> Just (bnot ctx)


--
-- * JSON serialization of BBN interface
--

instance ToJSON a => ToJSON (Dau (Ports a)) where
  toJSON d = object
    [ "GloballyUniqueId"   .= String (dauID d)
    , "Port"               .= listValue toJSON (ports d)
    , "BBNDauMonetaryCost" .= Number (fromInteger (monCost d)) ]

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
      pattrs = map entry (envToList (portAttrs p))
      entry (k,v) = k .= toJSON v

asPort :: ParseIt a -> ParseIt (Port a)
asPort asVal = do
    i <- key "GloballyUniqueId" asText
    fn <- key "BBNPortFunctionality" asText
    kvs <- eachInObject asVal 
    return (MkPort i fn (envFromList (filter isAttr kvs)))
  where
    exclude = ["GloballyUniqueId", "BBNPortFunctionality"]
    isAttr (k,_) = not (elem k exclude)

instance ToJSON Constraint where
  toJSON (Exactly v)   = toJSON v
  toJSON (OneOf vs)    = listValue toJSON vs
  toJSON (Range lo hi) = object [ "Min" .= toJSON lo, "Max" .= toJSON hi ]

asConstraint :: ParseIt Constraint
asConstraint = Exactly <$> asPVal
    <|> OneOf <$> eachInArray asPVal
    <|> Range <$> key "Min" asPVal <*> key "Max" asPVal

instance ToJSON SetInventory where
  toJSON s = object
    [ "daus" .= listValue toJSON (invDaus s) ]

asSetInventory :: ParseIt SetInventory
asSetInventory = MkSetInventory <$> key "daus" (eachInArray (asDau asConstraint))

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
-- * Driver
--

defaultMaxDaus :: Int
defaultMaxDaus   = 2

-- defaultIntervals :: Int
-- defaultIntervals = 2

defaultInventoryFile, defaultRequestFile, defaultResponseFile :: FilePath
defaultInventoryFile = "inbox/swap-inventory.json"
defaultRequestFile   = "inbox/swap-request.json"
defaultResponseFile  = "outbox/swap-response.json"

data SwapOpts = MkSwapOpts {
     swapRunSearch     :: Bool
   , swapMaxDaus       :: Int
   -- , swapMaxIntervals  :: Int
   , swapInventoryFile :: FilePath
   , swapRequestFile   :: FilePath
   , swapResponseFile  :: FilePath
} deriving (Data,Typeable,Generic,Eq,Read,Show)

parseSwapOpts :: Parser SwapOpts
parseSwapOpts = MkSwapOpts

    <$> switch
         ( long "run"
        <> help "Run the search for replacement DAUs" )

    <*> intOption 
         ( long "max-daus"
        <> value defaultMaxDaus
        <> help "Max number of DAUs to include in  response; 0 for no limit" )
    
    -- <*> intOption 
    --      ( long "range-intervals"
    --     <> value defaultIntervals
    --     <> help "Number of intervals to use for range constraints" )

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
  where
    intOption mods = option auto (mods <> showDefault <> metavar "INT")
    pathOption mods = strOption (mods <> showDefault <> metavar "FILE")

-- | Main driver.
runSwap :: SwapOpts -> IO ()
runSwap opts = do
    when (swapRunSearch opts) $ do
      req <- readJSON (swapRequestFile opts) asRequest
      MkSetInventory daus <- readJSON (swapInventoryFile opts) asSetInventory
      putStrLn "Searching for replacement DAUs ..."
      result <- findReplacement (swapMaxDaus opts) (createInventory daus) req
      case result of
        Just res -> do 
          let resFile = swapResponseFile opts
          writeJSON resFile res
          putStrLn ("Success. Response written to: " ++ resFile)
        Nothing -> do
          putStrLn "No replacement DAUs found."
          exitWith (ExitFailure 3)
