module DSL.Example.SwapDau.Search where

import Control.Monad.IO.Class (liftIO)
import Data.Function (on)
import Data.List (findIndex,foldl',nub,nubBy,sortBy,subsequences)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (isPrefixOf,pack,unpack)
import Z3.Monad (modelToString)

import DSL.Condition
import DSL.Environment
import DSL.Evaluation
import DSL.Pretty
import DSL.SAT
import DSL.Types
import DSL.Variational

import DSL.Example.SwapDau.Encoding
import DSL.Example.SwapDau.Types


--
-- * Search driver
--

-- | Find replacement DAUs in the given inventory.
findReplacement :: Int -> Rules -> Inventory -> Request -> IO (Metrics, Maybe Response)
findReplacement size rules inv req = do
    z3 <- initSolver
    let daus = toReplace req
    let invs = toSearch size daus inv
    let ports = buildPortMap daus
    let metrics = initMetrics daus (length inv) size (length invs)
    let test m i = do
          let model = appModel rules (provisions i) daus
          let dims = boolDims model
          let m' = addExplored i dims m
          syms <- symEnvFresh z3 dims Set.empty
          model' <- runSat z3 (prepare syms model)
          (_,s) <- runEval z3 envEmpty (initEnv i) (loadModel model' [])
          return (syms, m', s)
    let loop m []     = return (m, Nothing)
        loop m (i:is) = do
          (syms, m', s) <- test m i
          pass <- runSat z3 (condNot (errCtx s))
          ok <- runSat z3 (isSat (condSymOrFail pass))
          if ok then return (m', Just (i, syms, resEnv s, pass)) else loop m' is
    -- writeJSON "outbox/swap-model-debug.json" (appModel rules (provisions (invs !! 1)) daus)
    -- putStrLn $ "To replace: " ++ show daus
    -- putStrLn $ "Inventory: " ++ show inv
    (metrics', result) <- loop metrics invs
    case result of
      Nothing -> return (metrics', Nothing)
      Just (i, syms, renv, pass) -> runSat z3 $ do
        Just sol <- satModel (condSymOrFail pass)
        solStr <- modelToString sol
        liftIO $ writeFile "outbox/swap-solution.txt" solStr
        (cfg,_) <- satResult syms sol
        let replace = buildReplaceMap cfg
        -- putStrLn $ "Configuration: " ++ show (buildConfig r)
        -- putStrLn $ "Resource Env: " ++ show renv
        return (metrics', Just (buildResponse ports i renv replace cfg))


--
-- * Metrics
--

-- | Intitialize the metrics state.
initMetrics :: [Dau (PortGroups a)] -> Int -> Int -> Int -> Metrics
initMetrics reqDaus invSize maxDaus okSubs = MkMetrics {
      numReqDaus   = length reqDaus
    , numReqPorts  = sum (map groupSize reqGrps)
    , numReqGroups = length reqGrps
    , numInvDaus   = invSize
    , numInvs      = numSubs
    , numIgnored   = numSubs - okSubs
    , numExplored  = 0
    , numExpPorts  = 0
    , numExpGroups = 0
    , numCfgDims   = 0
    , numUseDims   = 0 }
  where
    reqGrps = concatMap ports reqDaus
    numSubs = sum (map (choose invSize) [0..maxDaus])
    choose n k = foldl (\z i -> (z * (n - i + 1)) `div` i) 1 [1..k]  -- binomial coefficient

-- | Add an explored sub-inventory.
addExplored :: Inventory -> Set Var -> Metrics -> Metrics
addExplored inv dims m = m {
      numExplored  = numExplored  m + 1
    , numExpPorts  = numExpPorts  m + sum (map groupSize invGrps)
    , numExpGroups = numExpGroups m + length invGrps
    , numCfgDims   = numCfgDims   m + numDims "Cfg"
    , numUseDims   = numUseDims   m + numDims "Use" }
  where
    invGrps = concatMap ports inv
    numDims pre = Set.size (Set.filter (isPrefixOf pre) dims)


--
-- * Defining the search space
--

-- * Constructing inventories

-- | Extract the list of DAUs to replace from a request and group their ports.
toReplace :: Request -> [Dau (PortGroups Constraint)]
toReplace = map (groupPortsInDau . reqDau) . filter replace . reqDaus

-- | Convert a list of ungrouped DAUs into a DAU inventory.
createInventory :: [Dau (Ports Constraint)] -> Inventory
createInventory ds = sortBy (compare `on` monCost) (map groupPortsInDau ds)

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

-- | The provision map for a given inventory.
provisions :: Inventory -> Provisions
provisions = envFromListAcc . concatMap dau
  where
    dau (MkDau n gs _) = map (grp n) (zip gs [1..])
    grp n (g,i) = (groupFunc g, [MkProvision n i g])


-- ** Filtering and ordering inventories

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

-- | Sort a list of inventories by monetary cost. Note that this forces
--   computing all subsequences of the inventory up front and so will
--   only scale to dictionaries of 20 or so relevant DAUs. If we instead
--   apply some basic heuristics (e.g. limit to 2-3 replacement DAUs), we
--   can lazily explore this space and so scale better.
sortInventories :: [Inventory] -> [Inventory]
sortInventories = sortBy (compare `on` inventoryCost)

-- | Filter inventory to include only DAUs that provide functionalities
--   relevant to the given port groups.
filterInventory :: PortGroups a -> Inventory -> Inventory
filterInventory gs ds = filter (any relevant . map groupFunc . ports) ds
  where
    fns = Set.fromList (map groupFunc gs)
    relevant fn = Set.member fn fns

-- | Monetary cost of a (sub-)inventory.
inventoryCost :: Inventory -> Int
inventoryCost = sum . map monCost

-- | All subsequences of a list up to a maximum length.
subsUpToLength :: Int -> [a] -> [[a]]
subsUpToLength k xs = [] : subs k xs
  where
    subs 0 _      = []
    subs _ []     = []
    subs k (x:xs) = map (x:) (subsUpToLength (k-1) xs) ++ subs k xs


--
-- * Building the response
--

-- | Mapping from DAU IDs and group indexes to the set of member port IDs.
type PortMap = Map (Name,Int) [Name]

-- | A nested map that describes which inventory DAUs and port groups
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
buildReplaceMap :: Env Var Bool -> ReplaceMap
buildReplaceMap dimEnv = foldr processDim Map.empty (envToList dimEnv)
  where
    processDim (_,False) daus = daus
    processDim (d,True)  daus
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
        | otherwise  = error ("buildReplaceMap: Unrecognized dimension: " ++ unpack d)
      where
        (k:l:r:_) = words (unpack d)
        split = break (== '+')

-- | Create a response.
buildResponse
  :: PortMap
  -> Inventory
  -> ResEnv
  -> ReplaceMap
  -> Env Var Bool
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
  -> Env Var Bool
  -> Dau (PortGroups Constraint)
  -> PortMap
  -> (PortMap, ResponseDau)
buildResponseDau renv rep cfg (MkDau d gs c) ports =
    (ports', MkResponseDau (maybe [] fst (Map.lookup d rep)) (MkDau d gs' c))
  where
    (ports', gs') = foldl' go (ports,[]) (zip gs [1..])
    go (ps,gs) (g,i) =
      let (ps', ns) = replacedPorts rep d i (groupSize g) ps
      in (ps', gs ++ expandAndConfig renv cfg ns d i g)

-- | Consume and return replaced ports.
replacedPorts
  :: ReplaceMap            -- ^ replacement-tracking map
  -> Name                  -- ^ inventory DAU ID
  -> Int                   -- ^ inventory group index
  -> Int                   -- ^ size of inventory group
  -> PortMap               -- ^ map of ports that need replacing
  -> (PortMap, [(Name,Int,[Name])])
replacedPorts rep invDau invGrp size ports
    | Just (_, grps) <- Map.lookup invDau rep
    , Just reqs <- Map.lookup invGrp grps
    = let go (ps,ns,k) r@(reqDau,reqGrp) = case Map.lookup r ps of
            Just ms ->
              ( Map.insert r (drop k ms) ps
              , (reqDau, reqGrp, take k ms) : ns
              , max 0 (k - length ms)
              )
            Nothing -> (ps,ns,k)
          (ports', names, _) = foldl' go (ports, [], size) reqs
      in (ports', reverse names)
    | otherwise = (ports,[])

-- | Expand and configure a port group.
expandAndConfig
  :: ResEnv                -- ^ final resource environment
  -> Env Var Bool          -- ^ configuration
  -> [(Name,Int,[Name])]   -- ^ replaced required ports w/ DAU ID & group index
  -> Name                  -- ^ inventory DAU ID
  -> Int                   -- ^ inventory group index
  -> PortGroup Constraint  -- ^ inventory port group
  -> [ResponsePort]
expandAndConfig renv cfg repl invDau invGrp (MkPortGroup invPorts f as _) = do
    (reqDau,reqGrp,reqPorts,invPorts') <- zipPorts repl invPorts
    let as' = configPortAttrs renv cfg invDau invGrp reqDau reqGrp as
    (old,new) <- zip reqPorts invPorts'
    return (MkResponsePort old (MkPort new f as'))
  where
    zipPorts [] _ = []
    zipPorts ((rn,rg,rps):t) ips =
      let k = length rps
      in (rn, rg, rps, take k ips) : zipPorts t (drop k ips)

-- | Configure port attributes based on the resource environment.
configPortAttrs
  :: ResEnv                -- ^ final resource environment
  -> Env Var Bool          -- ^ configuration
  -> Name                  -- ^ inventory DAU ID
  -> Int                   -- ^ inventory group index
  -> Name                  -- ^ required DAU ID
  -> Int                   -- ^ required group index
  -> PortAttrs Constraint  -- ^ inventory group's unconfigured attributes
  -> PortAttrs AttrVal
configPortAttrs renv cfg invDauID invGrpIx reqDauID reqGrpIx (MkPortAttrs (Env m)) =
    MkPortAttrs (Env (Map.mapWithKey (config pre) m))
  where
    pre = provPrefix invDauID invGrpIx reqDauID reqGrpIx <> "Attributes"
    dimGen = dimAttr invDauID invGrpIx reqDauID reqGrpIx
    path (ResID ns) a = ResID (ns ++ [a])
    config p a (Sync as) =
      let ds = take (length as - 1) (dimN (dimGen a))
          ix = fromMaybe 0 $ findIndex (\d -> envLookupOrFail "configPortAttrs" d cfg) ds
          MkPortAttrs (Env m) = as !! ix
      in Node (MkPortAttrs (Env (Map.mapWithKey (config (path p a)) m)))
    config p a _ = case envLookup (path p a) renv of
      Just v -> case configure cfg envEmpty v of
        One (Just pv) -> Leaf pv
        One Nothing ->
          error $ "Misconfigured attribute: " ++ prettyString (path p a)
            ++ "\n  started with: " ++ prettyString v
            ++ "\n  configured with: " ++ show cfg
        _ -> error $ "Internal error: choice after configuration: " ++ prettyString v
      Nothing -> error ("Missing attribute: " ++ prettyString (path p a))
