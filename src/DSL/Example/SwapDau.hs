{-# OPTIONS_GHC -Wno-type-defaults #-}

module DSL.Example.SwapDau where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Control.Monad (when)
import Data.Aeson hiding (Value)
import Data.Aeson.BetterErrors
import Data.Aeson.Types (Pair, listValue)
import Data.Function (on)
import Data.List (findIndex,foldl',nub,nubBy,sortBy,subsequences)
import Data.Maybe (fromMaybe)
import Data.SBV (AllSatResult,Boolean(..),getModelDictionaries)
import Data.SBV.Internals (trueCW)
import Data.String (fromString)
import Options.Applicative hiding ((<|>))
import System.Exit

import Data.Text (pack)
import qualified Data.Text as Text

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

import DSL.Environment
import DSL.Model
import DSL.Name
import DSL.Parser (parseExprText)
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
--   the values of the port attributes will be of type 'AttrVal', while for an
--   unconfigured DAU, the values of port attributes will be 'Constraint',
--   which captures the range of possible values the port can take on.
newtype PortAttrs a = MkPortAttrs (Env Name a)
  deriving (Data,Typeable,Generic,Eq,Show,Functor)

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
} deriving (Data,Typeable,Generic,Eq,Show)

-- | A constraint on a port attribute in an unconfigured DAU.
data Constraint
   = Exactly PVal
   | OneOf [PVal]
   | Range Int Int
   | Sync [PortAttrs Constraint]
  deriving (Data,Typeable,Generic,Eq,Show)

-- | A value of a port attribute in a configured DAU.
data AttrVal
   = Leaf PVal
   | Node (PortAttrs AttrVal)
  deriving (Data,Typeable,Generic,Eq,Show)


-- ** Attribute rules

-- | A rule refines the check associated with a particular attribute. There
--   are two kinds of rules: Equations define derived attributes whose values
--   are determined by an arithmetic expression over other attribute values.
--   Compatibility rules describe a list of values that are considered
--   compatible with a given value of a given attribute.
data Rule
   = Compatible [PVal]
   | Equation Expr
  deriving (Data,Typeable,Generic,Eq,Show)

-- | A set of rules that apply to a given attribute-value pair.
newtype Rules = MkRules (Env (Name,PVal) Rule)
  deriving (Eq,Show,Data)


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
--   -- TODO: Make this a set so ordering of JSON file doesn't matter
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

-- | A port in an adaptation response, which replaces another named port.
data ResponsePort = MkResponsePort {
     oldPort :: Name
   , resPort :: Port AttrVal
} deriving (Data,Typeable,Generic,Eq,Show)

-- | A DAU in an adaptation response, which replaces one or more DAUs in the
--   corresponding request, whose ports are configured to single values.
data ResponseDau = MkResponseDau {
     oldDaus :: [Name]
   , resDau  :: Dau [ResponsePort]
} deriving (Data,Typeable,Generic,Eq,Show)



--
-- * Helper functions
--

-- | Convert a set of port attributes to a list of name-value pairs.
portAttrsToList :: PortAttrs a -> [(Name,a)]
portAttrsToList (MkPortAttrs m) = envToList m

-- | Convert a list of name-value pairs to a set of port attributes.
portAttrsFromList :: [(Name,a)] -> PortAttrs a
portAttrsFromList = MkPortAttrs . envFromList


--
-- * Port grouping
--

-- | Group ports with identical attributes together within a DAU.
groupPortsInDau :: Eq a => Dau (Ports a) -> Dau (PortGroups a)
groupPortsInDau (MkDau n ps c) = MkDau n (groupPorts ps) c

-- | Group ports with identical attributes together.
groupPorts :: Eq a => Ports a -> PortGroups a
groupPorts ps = do
    u <- unique
    let qs = filter (same u) ps
    return (MkPortGroup (map portID qs) (portFunc u) (portAttrs u) (length qs))
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
dimAttr dau grp att = mconcat ["Cfg ", dau, "+", pack (show grp), " ", att]

-- | Dimension indicating whether a provided DAU + group is used to (partially)
--   satisfy a required DAU + port group.
dimUseGroup :: Name -> Int -> Name -> Int -> Var
dimUseGroup provDau provGrp reqDau reqGrp =
    mconcat ["Use ", provDau, "+", pack (show provGrp), " ", reqDau, "+", pack (show reqGrp)]


-- ** Provisions

-- | Translate a DAU inventory into a dictionary.
toDictionary :: Rules -> Inventory -> Dictionary
toDictionary rules = envFromList . map entry
  where
    entry d = (mkSymbol (dauID d), ModEntry (provideDau rules d))

-- | Encode a provided DAU as a DSL model.
provideDau :: Rules -> Dau (PortGroups Constraint) -> Model
provideDau rules (MkDau n gs c) = Model []
    [ Elems [
        modify "/MonetaryCost" TInt (val + fromInteger c)
      , In (Path Relative [n])
        [ Elems (zipWith (providePortGroup rules n) gs [1..]) ]
    ]]

-- | Encode a provided port group as a DSL statement.
providePortGroup :: Rules -> Name -> PortGroup Constraint -> Int -> Stmt
providePortGroup rules dau g i =
    In (fromString ("Group/" ++ show i))
    [ Elems [
      create "Functionality" (sym (groupFunc g))
    , create "PortCount" (lit (I (groupSize g)))
    , In "Attributes" (providePortAttrs rules dau i (groupAttrs g))
    ]]

-- | Encode a set of provided port attributes as a DSL statement block.
providePortAttrs :: Rules -> Name -> Int -> PortAttrs Constraint -> Block
providePortAttrs rules@(MkRules rs) dau i as =
    [ Elems $ [providePortAttr (dimAttr dau i n) n c | (n,c) <- attrs]
        ++ map (uncurry providePortEquation) eqns
    ]
  where
    (attrs,eqns) = foldr split ([],[]) (portAttrsToList as)
    split (n, Exactly v) (as,es)
      | Right (Equation e) <- envLookup (n,v) rs = (as, (n,e):es)
    split (n, OneOf [v]) (as,es)
      | Right (Equation e) <- envLookup (n,v) rs = (as, (n,e):es)
    split (n, c) (as,es) = ((n,c):as, es)

    providePortAttr dim att c =
      let path = Path Relative [att] in
      case c of
        Exactly v   -> Do path $ Create (lit v)
        OneOf vs    -> Do path $ Create (One (Lit (chcN (dimN dim) vs)))
        Range lo hi -> Do path $ Create (One (Lit (chcN (dimN dim) (map I [lo..hi]))))
        Sync ss     -> In path $ splitN (dimN dim) (map (providePortAttrs rules dau i) ss)
    
-- | Encode a provided port attribute that is defined via an equation.
providePortEquation :: Name -> Expr -> Stmt
providePortEquation att e = Do (Path Relative [att]) (Create (One e))


-- ** Requirements

-- | Check all required DAUs against the inventory of provisions.
requireDaus :: Rules -> Inventory -> [Dau (PortGroups Constraint)] -> [Stmt]
requireDaus rules inv reqs = concatMap (requireDau rules inv) reqs

-- | Check a required DAU against the inventory of provisions.
requireDau :: Rules -> Inventory -> Dau (PortGroups Constraint) -> [Stmt]
requireDau rules inv req = do
    (g,i) <- zip (ports req) [1..]
    requirePortGroup rules inv (dauID req) i g
        
-- | Check whether the required port group is satisfied by the inventory
--   of provisions; adjust the ports-to-match and available ports accordingly.
requirePortGroup
  :: Rules                 -- ^ attribute compatibility rules
  -> Inventory             -- ^ provided DAU inventory
  -> Name                  -- ^ required DAU name
  -> Int                   -- ^ index of required group
  -> PortGroup Constraint  -- ^ required group
  -> [Stmt]
requirePortGroup rules inv reqDauName reqGrpIx reqGrp =
    -- keep track of the ports we still have to match for this group
    (modify "/PortsToMatch" TInt (lit (I (groupSize reqGrp))) : do
       provDau <- inv
       let provDauName = dauID provDau
       provGrpIx <- [1 .. length (ports provDau)]
       let dim = dimUseGroup provDauName provGrpIx reqDauName reqGrpIx
       return $
         In (Path Relative [provDauName, "Group", pack (show provGrpIx)])
         [ Elems [checkGroup rules dim reqGrp] ])
    ++ [check "/PortsToMatch" tInt (val .== 0)]
        
-- | Check a required port group against the port group in the current context.
checkGroup :: Rules -> Var -> PortGroup Constraint -> Stmt
checkGroup rules dim grp =
    -- if there are ports left to match, ports left in this group,
    -- and this group provides the right functionality
    If (res "/PortsToMatch" .> 0
         &&& res "PortCount" .> 0
         &&& res "Functionality" .==. sym (groupFunc grp))
    [ Split (BRef dim)  -- tracks if this group was used for this requirement
      [ Elems [
        -- check all of the attributes
        In "Attributes" [ Elems (requirePortAttrs rules (groupAttrs grp)) ]
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
     ] []

-- | Check whether a required set of port attributes is satisfied in the
--   current context.
requirePortAttrs :: Rules -> PortAttrs Constraint -> [Stmt]
requirePortAttrs rules@(MkRules rs) as = do
    (n,c) <- portAttrsToList as
    let path = Path Relative [n]
    return $ case c of
      Exactly v ->
        let t = primType v
        in check path (One t) (oneOf t (compatible n v))
      OneOf vs ->
        let t = primType (head vs)
        in check path (One t) (oneOf t (concatMap (compatible n) vs))
      Range lo hi ->
        check path (One TInt) (val .>= int lo &&& val .<= int hi)
      Sync [as] ->
        In path [ Elems (requirePortAttrs rules as) ]
      Sync _ ->
        error "requirePortAttrs: we don't support choices of synchronized attributes in requirements."
  where
    eq t a b = case t of
      TUnit   -> true
      TBool   -> One (P2 (BB_B Eqv) a b)
      TInt    -> One (P2 (NN_B Equ) a b)
      TFloat  -> One (P2 (NN_B Equ) a b)
      TSymbol -> One (P2 (SS_B SEqu) a b)
    compatible n v = case envLookup (n,v) rs of
      Right (Compatible vs) -> vs
      _ -> [v]
    oneOf t vs = foldr1 (|||) [eq t val (lit v) | v <- nub vs]
    

-- ** Application model

-- | Generate the application model for a given inventory and set of DAUs to
--   replace.
appModel :: Rules -> Inventory -> [Dau (PortGroups Constraint)] -> Model
appModel rules inv daus = Model []
    [ Elems $
       create "/MonetaryCost" 0
    :  create "/PortsToMatch" 0
    :  [Load (sym (dauID d)) [] | d <- inv]
    ++ requireDaus rules inv daus
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
    configPort (MkPort i fn as) = MkResponsePort i (MkPort i fn (fmap configAttr as))
    configAttr (Exactly v)  = Leaf v
    configAttr (OneOf vs)   = Leaf (head vs)
    configAttr (Range v _)  = Leaf (I v)
    configAttr (Sync ss)    = Node (fmap configAttr (head ss))


-- ** Defining the search space

-- | Extract the list of DAUs to replace from a request and group their ports.
toReplace :: Request -> [Dau (PortGroups Constraint)]
toReplace = map (groupPortsInDau . reqDau) . filter replace . reqDaus

-- | Generate sub-inventories to search, filtered based on the given list of
--   DAUs to replace and an integer indicating the maximum size of each
--   sub-inventory. A max size less than 1 indicates unbounded sub-inventory
--   size (which will be slow for large inventories).
toSearch :: Int -> [Dau (PortGroups Constraint)] -> Inventory -> [Inventory]
toSearch size daus inv = map (free ++) $ sortInventories
    ((if size > 0 then subsUpToLength size else subsequences) nonFree)
  where
    filtered = filterInventory (concatMap ports daus) inv
    (free,nonFree) = foldr splitFree ([],[]) filtered
    splitFree d (f,n)
      | monCost d <= 0 = (d:f, n)
      | otherwise      = (f, d:n)


-- ** Building the response

-- | Mapping from DAU IDs and group indexes to the set of member port IDs.
type PortMap = Map (Name,Int) [Name]

-- | A nested maps that describes which inventory DAUs and port groups
--   replace which request DAUs and port groups. The keys of the outer map
--   are inventory DAU IDs, keys of the inner map are inventory group IDs.
--   values are a list. Values of the outer map include a list of request
--   DAUs the corresponding inventory DAU replaces, while values of the
--   inner maps include a list of request DAU groups the corresponding
--   group is replacing.
type ReplaceMap = Map Name ([Name], Map Int [(Name,Int)])

-- | Build a map of ports that need replacing from the grouped request DAUs.
buildPortMap :: [Dau (PortGroups a)] -> PortMap
buildPortMap ds = Map.fromList $ do
    d <- ds
    (g,i) <- zip (ports d) [1..]
    return ((dauID d, i), groupIDs g)

-- | Build a map describing which inventory DAUs and groups replaced which
--   request DAUs and groups from the output of the SAT solver.
buildReplaceMap :: AllSatResult -> ReplaceMap
buildReplaceMap = foldr processDim Map.empty
    . Map.keys . Map.filter (== trueCW) . head . getModelDictionaries
  where
    processDim dim daus
        | k == "Use" =
            let (ldau,_:lgrp) = split l
                (rdau,_:rgrp) = split r
                invDau = pack ldau
                invGrp = read lgrp
                reqDau = pack rdau
                reqGrp = read rgrp
            in flip (Map.insert invDau) daus $
               case Map.lookup invDau daus of
                 Nothing -> ([reqDau], Map.singleton invGrp [(reqDau,reqGrp)])
                 Just (ns,grps) ->
                   case Map.lookup invGrp grps of
                     Nothing ->
                       (nub (reqDau : ns), Map.insert invGrp [(reqDau,reqGrp)] grps)
                     Just gs ->
                       (nub (reqDau : ns), Map.insert invGrp ((reqDau,reqGrp):gs) grps)
        | k == "Cfg" = daus
        | otherwise  = error ("buildReplaceMap: Unrecognized dimension: " ++ dim)
      where
        [k,l,r] = words dim
        split = break (== '+')

-- | Build a configuration from the output of the SAT solver.
buildConfig :: AllSatResult -> Set Name
buildConfig = foldr processDim Set.empty
    . Map.assocs . head . getModelDictionaries
  where
    processDim (dim,v) cfg
        | k == "Use" = cfg
        | k == "Cfg" = if v == trueCW then Set.insert (fromString dim) cfg else cfg
        | otherwise  = error ("buildConfig: Unrecognized dimension: " ++ dim)
      where
        (k:_) = words dim

-- | Create a response.
buildResponse
  :: PortMap
  -> Inventory
  -> ResEnv
  -> ReplaceMap
  -> Set Name
  -> Response
buildResponse ports inv renv rep cfg =
    MkResponse (snd (foldl' go (ports,[]) inv))
  where
    go (ps,ds) dau = let (ps',d) = buildResponseDau renv rep cfg dau ps
                     in (ps',d:ds)

-- | Create a response DAU from an inventory DAU, consuming ports from the port
--   map as required.
buildResponseDau
  :: ResEnv
  -> ReplaceMap
  -> Set Name
  -> Dau (PortGroups Constraint)
  -> PortMap
  -> (PortMap, ResponseDau)
buildResponseDau renv rep cfg (MkDau d gs c) ports =
    (ports', MkResponseDau (maybe [] fst (Map.lookup d rep)) (MkDau d gs' c))
  where
    (ports', gs') = foldl' go (ports,[]) (zip gs [1..])
    go (ps,gs) (g,i) =
      let (ps',ns) = replacedPortNames rep d i (groupSize g) ps
      in (ps', gs ++ expandAndConfig renv cfg ns d i g)

-- | Consume and return replaced port names.
replacedPortNames
  :: ReplaceMap
  -> Name
  -> Int
  -> Int
  -> PortMap
  -> (PortMap, [Name])
replacedPortNames rep invDau invGrp size ports
    | Just (_, grps) <- Map.lookup invDau rep
    , Just reqs <- Map.lookup invGrp grps
    = let go (ps,ns,k) r = case Map.lookup r ps of
            Just ms | length ms <= k -> (Map.insert r [] ps, ns ++ ms, k - length ms)
                    | otherwise      -> (Map.insert r (drop k ms) ps, ns ++ take k ms, 0)
            Nothing -> (ps,ns,k)
          (ports', names, _) = foldl' go (ports, [], size) reqs
      in (ports', names)
    | otherwise = (ports,[])

-- | Expand and configure a port group.
expandAndConfig
  :: ResEnv
  -> Set Name
  -> [Name]
  -> Name
  -> Int
  -> PortGroup Constraint
  -> [ResponsePort]
expandAndConfig renv cfg old d i (MkPortGroup new f as _) = do
    (n,o) <- zip new (old ++ repeat "")
    return (MkResponsePort o (MkPort n f as'))
  where
    as' = configPortAttrs renv cfg d i as

-- | Configure port attributes based on the resource environment.
configPortAttrs
  :: ResEnv
  -> Set Name
  -> Name
  -> Int
  -> PortAttrs Constraint
  -> PortAttrs AttrVal
configPortAttrs renv cfg d i (MkPortAttrs (Env m)) =
    MkPortAttrs (Env (Map.mapWithKey (config pre) m))
  where
    pre = ResID [d, "Group", fromString (show i), "Attributes"]
    path (ResID ns) a = ResID (ns ++ [a])
    syncIx a n = fromMaybe 0 $ findIndex (flip Set.member cfg) (take n (dimN (dimAttr d i a)))
    config p a (Sync as) =
      let MkPortAttrs (Env m) = as !! syncIx a (length as)
      in Node (MkPortAttrs (Env (Map.mapWithKey (config (path p a)) m)))
    config p a _ = case envLookup (path p a) renv of
      Right v -> case fullConfig cfg v of
        Just pv -> Leaf pv
        Nothing -> error $ "Misconfigured attribute: " ++ show (path p a)
                     ++ "\n  started with: " ++ show v
                     ++ "\n  configured with: " ++ show cfg
      Left err -> error (show err)

-- | Fully configure a value by interpreting every dimension as either 'True'
--   or 'False'. Any dimension in the argument set is interpreted as 'True',
--   otherwise it's interpreted as 'False'.
--   TODO: This should probably be a method of the 'Select' type class.
fullConfig :: Set Name -> Value -> Maybe PVal
fullConfig _   (One v)     = v
fullConfig cfg (Chc c l r) = fullConfig cfg (if bexpr c then l else r)
  where
    bexpr (BLit b)     = b
    bexpr (BRef d)     = Set.member d cfg
    bexpr (OpB o e)    = opB_B o (bexpr e)
    bexpr (OpBB o l r) = opBB_B o (bexpr l) (bexpr r)
    bexpr e = error $ "Unexpected condition: " ++ show e


-- ** Main driver

-- | Find replacement DAUs in the given inventory.
findReplacement :: Int -> Rules -> Inventory -> Request -> IO (Maybe Response)
findReplacement mx rules inv req = do
    -- writeJSON "outbox/swap-dictionary-debug.json" dict
    -- writeJSON "outbox/swap-model-debug.json" (appModel rules (invs !! 1) daus)
    -- putStrLn $ "To replace: " ++ show daus
    -- putStrLn $ "Inventory: " ++ show inv
    case loop invs of
      Nothing -> return Nothing
      Just (i, renv, ctx) -> do 
        r <- satResults 1 ctx
        writeFile "outbox/swap-solution.txt" (show r)
        return (Just (buildResponse ports i renv (buildReplaceMap r) (buildConfig r)))
  where
    dict = toDictionary rules inv
    daus = toReplace req
    invs = toSearch mx daus inv
    ports = buildPortMap daus
    test i = runWithDict dict envEmpty (loadModel (appModel rules i daus) [])
    loop []     = Nothing
    loop (i:is) = case test i of
        -- (Left _, s) -> traceShow s (loop is)
        (Left _, _) -> loop is
        (Right _, SCtx renv ctx _) -> Just (i, renv, bnot ctx)


--
-- * JSON serialization of BBN interface
--

instance ToJSON Rules where
  toJSON (MkRules m) = listValue rule (envToList m)
    where
      rule ((a,v),r) = object
        [ "Attribute" .= String a
        , "AttributeValue" .= toJSON v
        , case r of
            Compatible vs -> "Compatible" .= listValue toJSON vs
            Equation e    -> "Equation"   .= toJSON e
        ]

asRules :: ParseIt Rules
asRules = eachInArray rule >>= return . MkRules . envFromList
  where
    rule = do
      a <- key "Attribute" asText
      v <- key "AttributeValue" asPVal
      r <- Compatible <$> key "Compatible" (eachInArray asPVal)
             <|> Equation <$> key "Equation" asExpr'
      return ((a,v),r)
    -- TODO Alex's revised parser doesn't handle spaces right...
    -- this is a customized version of asExpr that works around this
    asExpr' = do
      t <- asText
      let t' = Text.filter (/= ' ') t
      case parseExprText t' of
        Right e  -> pure e
        Left msg -> throwCustomError (ExprParseError (pack msg) t)

instance ToJSON p => ToJSON (Dau p) where
  toJSON d = object
    [ "GloballyUniqueId"   .= String (dauID d)
    , "Port"               .= toJSON (ports d)
    , "BBNDauMonetaryCost" .= Number (fromInteger (monCost d)) ]

asDau :: ParseIt a -> ParseIt (Dau (Ports a))
asDau asVal = do
    i <- key "GloballyUniqueId" asText
    ps <- key "Port" (eachInArray (asPort asVal))
    mc <- key "BBNDauMonetaryCost" asIntegral
    return (MkDau i ps mc)

portAttrsPairs :: ToJSON a => PortAttrs a -> [Pair]
portAttrsPairs = map entry . portAttrsToList
    where
      entry (k,v) = k .= toJSON v

asPortAttrs :: ParseIt a -> ParseIt (PortAttrs a)
asPortAttrs asVal = do
    kvs <- eachInObject asVal
    return (portAttrsFromList (filter isAttr kvs))
  where
    exclude = ["GloballyUniqueId", "BBNPortFunctionality"]
    isAttr (k,_) = not (elem k exclude)

instance ToJSON a => ToJSON (Port a) where
  toJSON p = object (pid : pfn : pattrs)
    where
      pid = "GloballyUniqueId" .= portID p
      pfn = "BBNPortFunctionality" .= portFunc p
      pattrs = portAttrsPairs (portAttrs p)

asPort :: ParseIt a -> ParseIt (Port a)
asPort asVal = do
    i <- key "GloballyUniqueId" asText
    fn <- key "BBNPortFunctionality" asText
    as <- asPortAttrs asVal
    return (MkPort i fn as)

instance ToJSON Constraint where
  toJSON (Exactly v)   = toJSON v
  toJSON (OneOf vs)    = listValue toJSON vs
  toJSON (Range lo hi) = object [ "Min" .= toJSON lo, "Max" .= toJSON hi ]
  toJSON (Sync ss)     = listValue (object . portAttrsPairs) ss

asConstraint :: ParseIt Constraint
asConstraint = Exactly <$> asPVal
    <|> OneOf <$> eachInArray asPVal
    <|> Range <$> key "Min" asInt <*> key "Max" asInt
    <|> Sync  <$> eachInArray (asPortAttrs asConstraint)
    <|> Sync . (:[]) <$> asPortAttrs asConstraint

instance ToJSON AttrVal where
  toJSON (Leaf v)  = toJSON v
  toJSON (Node as) = object (portAttrsPairs as)

asAttrVal :: ParseIt AttrVal
asAttrVal = Leaf <$> asPVal
    <|> Node <$> asPortAttrs asAttrVal

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

instance ToJSON ResponseDau where
  toJSON r = case toJSON (resDau r) of
      Object o -> Object (attr <> o)
      _ -> error "ResponseDau#toJSON: internal error"
    where
      attr = "SupersededGloballyUniqueIds" .= listValue String (oldDaus r)

instance ToJSON ResponsePort where
  toJSON p = case toJSON (resPort p) of
      Object o -> Object (attr <> o)
      _ -> error "ResponsePort#toJSON: internal error"
    where
      attr = "SupersededGloballyUniqueId" .= oldPort p


--
-- * Driver
--

defaultMaxDaus :: Int
defaultMaxDaus   = 2

defaultRulesFile, defaultInventoryFile, defaultRequestFile, defaultResponseFile :: FilePath
defaultRulesFile     = "inbox/swap-rules.json"
defaultInventoryFile = "inbox/swap-inventory.json"
defaultRequestFile   = "inbox/swap-request.json"
defaultResponseFile  = "outbox/swap-response.json"

data SwapOpts = MkSwapOpts {
     swapRunSearch     :: Bool
   , swapMaxDaus       :: Int
   , swapRulesFile     :: FilePath
   , swapInventoryFile :: FilePath
   , swapRequestFile   :: FilePath
   , swapResponseFile  :: FilePath
} deriving (Data,Typeable,Generic,Eq,Read,Show)

defaultOpts :: SwapOpts
defaultOpts = MkSwapOpts True 2 defaultRulesFile defaultInventoryFile defaultRequestFile defaultResponseFile

parseSwapOpts :: Parser SwapOpts
parseSwapOpts = MkSwapOpts

    <$> switch
         ( long "run"
        <> help "Run the search for replacement DAUs" )

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
  where
    intOption mods = option auto (mods <> showDefault <> metavar "INT")
    pathOption mods = strOption (mods <> showDefault <> metavar "FILE")

-- | Top-level driver.
runSwap :: SwapOpts -> IO ()
runSwap opts = do
    when (swapRunSearch opts) $ do
      req <- readJSON (swapRequestFile opts) asRequest
      MkSetInventory daus <- readJSON (swapInventoryFile opts) asSetInventory
      rules <- readJSON (swapRulesFile opts) asRules
      putStrLn "Searching for replacement DAUs ..."
      result <- findReplacement (swapMaxDaus opts) rules (createInventory daus) req
      case result of
        Just res -> do 
          let resFile = swapResponseFile opts
          writeJSON resFile res
          putStrLn ("Success. Response written to: " ++ resFile)
        Nothing -> do
          putStrLn "No replacement DAUs found."
          exitWith (ExitFailure 3)

-- | A simplified driver suitable for testing.
runSwapTest :: SwapOpts -> IO (Maybe Response)
runSwapTest opts = do
    req <- readJSON (swapRequestFile opts) asRequest
    MkSetInventory daus <- readJSON (swapInventoryFile opts) asSetInventory
    rules <- readJSON (swapRulesFile opts) asRules
    findReplacement (swapMaxDaus opts) rules (createInventory daus) req
