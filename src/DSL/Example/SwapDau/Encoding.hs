{-# OPTIONS_GHC -Wno-type-defaults #-}

-- | DSL encoding of the DAU swapping challenge problem.
module DSL.Example.SwapDau.Encoding where

import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (pack)

import DSL.Boolean
import DSL.Environment
import DSL.Evaluation
import DSL.Path
import DSL.Primitive
import DSL.Sugar
import DSL.Types

import DSL.Example.SwapDau.Types


--
-- * DSL encoding
--

-- ** Dimension names

-- | Generate an infinite sequence of dimensions from the given prefix. Used
--   for choosing among several possibilities when configuring port attributes.
dimN :: Var -> [Var]
dimN pre = [mconcat [pre, "+", pack (show i)] | i <- [1..]]

-- | Dimension indicating how to configure a port attribute.
dimAttr
  :: Name  -- ^ inventory DAU ID
  -> Int   -- ^ inventory group index
  -> Name  -- ^ required DAU ID
  -> Int   -- ^ required group index
  -> Name  -- ^ attribute name
  -> Var
dimAttr invDau invGrp reqDau reqGrp att = mconcat 
    [ "Cfg "
    , invDau, "+", pack (show invGrp), " "
    , reqDau, "+", pack (show reqGrp), " "
    , att]

-- | Dimension indicating whether a provided DAU + group is used to (partially)
--   satisfy a required DAU + port group.
dimUseGroup
  :: Name  -- ^ inventory DAU ID
  -> Int   -- ^ inventory group index
  -> Name  -- ^ required DAU ID
  -> Int   -- ^ required group index
  -> Var
dimUseGroup invDau invGrp reqDau reqGrp = mconcat
    [ "Use "
    , invDau, "+", pack (show invGrp), " "
    , reqDau, "+", pack (show reqGrp)]


-- ** Provisions

-- | Resource path prefix of an inventory port group.
invPrefix :: Name -> Int -> ResID
invPrefix invDau invGrp =
    ResID [invDau, fromString (show invGrp)]

-- | Resource path prefix of a set of provisions.
provPrefix :: Name -> Int -> Name -> Int -> ResID
provPrefix invDau invGrp reqDau reqGrp =
    ResID [invDau, fromString (show invGrp), reqDau, fromString (show reqGrp)]

-- | Initial resource environment for a given inventory.
initEnv :: Inventory -> ResEnv
initEnv inv = envFromList (cost : match : concatMap dau inv)
  where
    cost = ("/MonetaryCost", One (Just (I (sum (map monCost inv)))))
    match = ("/PortsToMatch", One (Just (I 0)))
    dau (MkDau n gs _) = map (grp n) (zip gs [1..])
    grp n (g,i) = (invPrefix n i <> "PortCount", One (Just (I (groupSize g))))

-- | Provide a port group for use by a particular required port group.
providePortGroup
  :: Rules                 -- ^ shared attribute rules
  -> Name                  -- ^ inventory DAU ID
  -> Int                   -- ^ inventory group index
  -> Name                  -- ^ required DAU ID
  -> Int                   -- ^ required group index
  -> PortGroup Constraint  -- ^ provided port group
  -> Stmt
providePortGroup rules invDauID invGrpIx reqDauID reqGrpIx grp =
    In "Attributes" (providePortAttrs rules dimGen (groupAttrs grp))
  where
    dimGen = dimAttr invDauID invGrpIx reqDauID reqGrpIx

-- | Encode a set of provided port attributes as a DSL statement block.
providePortAttrs
  :: Rules                 -- ^ shared attribute rules
  -> (Name -> Var)         -- ^ unique dimension generator
  -> PortAttrs Constraint  -- ^ provided attributes
  -> Block
providePortAttrs rules@(MkRules rs) dimGen as =
    [providePortAttr (dimGen n) n c | (n,c) <- attrs]
    ++ map (uncurry providePortEquation) eqns
  where
    (attrs,eqns) = foldr split ([],[]) (portAttrsToList as)
    split (n, Exactly v) (as,es)
      | Just (Equation e) <- envLookup (n,v) rs = (as, (n,e):es)
    split (n, OneOf [v]) (as,es)
      | Just (Equation e) <- envLookup (n,v) rs = (as, (n,e):es)
    split (n, c) (as,es) = ((n,c):as, es)

    providePortAttr dim att c =
      let path = Path Relative [att] in
      case c of
        Exactly v   -> Do path $ Create (lit v)
        OneOf vs    -> Do path $ Create (One (Lit (chcN (dimN dim) vs)))
        Range lo hi -> Do path $ Create (One (Lit (chcN (dimN dim) (map I [lo..hi]))))
        Sync ss     -> In path $ splitN (dimN dim) (map (providePortAttrs rules dimGen) ss)
    
-- | Encode a provided port attribute that is defined via an equation.
providePortEquation :: Name -> Expr -> Stmt
providePortEquation att e = Do (Path Relative [att]) (Create (One e))


-- ** Requirements

-- | Check all required DAUs against the provisions.
requireDaus :: Rules -> Provisions -> [Dau (PortGroups Constraint)] -> [Stmt]
requireDaus rules prov reqs = concatMap (requireDau rules prov) reqs

-- | Check a required DAU against the provisions.
requireDau :: Rules -> Provisions -> Dau (PortGroups Constraint) -> [Stmt]
requireDau rules prov req = do
    (g,i) <- zip (ports req) [1..]
    requirePortGroup rules prov (dauID req) i g

-- | Check whether the required port group is satisfied by the provisions;
--   adjust the ports-to-match and available ports accordingly.
requirePortGroup
  :: Rules                 -- ^ attribute compatibility rules
  -> Provisions            -- ^ provided port groups
  -> Name                  -- ^ required DAU name
  -> Int                   -- ^ index of required group
  -> PortGroup Constraint  -- ^ required group
  -> [Stmt]
requirePortGroup rules prov reqDauID reqGrpIx reqGrp =
    -- keep track of the ports we still have to match for this group
    ( modify "/PortsToMatch" TInt (lit (I (groupSize reqGrp))) : do
        MkProvision provDauID provGrpIx provGrp <- relevant
        let dim = dimUseGroup provDauID provGrpIx reqDauID reqGrpIx
        let provPath = toPath (provPrefix provDauID provGrpIx reqDauID reqGrpIx)
        let portCount = toPath (invPrefix provDauID provGrpIx <> "PortCount")
        return $
          In provPath
            [ providePortGroup rules provDauID provGrpIx reqDauID reqGrpIx provGrp
            , checkGroup rules dim portCount reqGrp
            ]
    ) ++ [check "/PortsToMatch" TInt (val .== 0)]
  where
    relevant = fromMaybe [] (envLookup (groupFunc reqGrp) prov)
        
-- | Check a required port group against the port group in the current context.
checkGroup :: Rules -> Var -> Path -> PortGroup Constraint -> Stmt
checkGroup rules dim portCount grp =
    -- if there are ports left to match, ports left in this group
    If (res "/PortsToMatch" .> 0 &&& res portCount .> 0)
    [ If (chc dim true false) -- tracks if this group was used for this requirement
      [ -- check all of the attributes
        In "Attributes" (requirePortAttrs rules (groupAttrs grp))
        -- if success, update the ports available and required
      , If (res "/PortsToMatch" .> res portCount)
        [ modify "/PortsToMatch" TInt (val - res portCount)
        , modify portCount TInt 0
        ]
        [ modify portCount TInt (val - res "/PortsToMatch")
        , modify "/PortsToMatch" TInt 0
        ]
      ] []
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
        in check path t (oneOf t (compatible n v))
      OneOf vs ->
        let t = primType (head vs)
        in check path t (oneOf t (concatMap (compatible n) vs))
      Range lo hi ->
        check path TInt (val .>= int lo &&& val .<= int hi)
      Sync [as] ->
        In path (requirePortAttrs rules as)
      Sync _ ->
        error "requirePortAttrs: we don't support choices of synchronized attributes in requirements."
  where
    eq t a b = case t of
      TUnit   -> true
      TBool   -> One (P2 (BB_B Eqv) a b)
      TInt    -> One (P2 (NN_B Equ) a b)
      TFloat  -> One (P2 (NN_B Equ) a b)
      TString -> One (P2 (SS_B SEqu) a b)
    compatible n v = case envLookup (n,v) rs of
      Just (Compatible vs) -> vs
      _ -> [v]
    oneOf t vs = foldr1 (|||) [eq t val (lit v) | v <- nub vs]
    

-- ** Application model

-- | Generate the application model.
appModel :: Rules -> Provisions -> [Dau (PortGroups Constraint)] -> Model
appModel rules prov daus = Model [] (requireDaus rules prov daus)
