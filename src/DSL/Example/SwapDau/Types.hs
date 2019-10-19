module DSL.Example.SwapDau.Types where

import Data.Data (Typeable)
import GHC.Generics (Generic)

import DSL.Environment
import DSL.Types

--
-- * DAUs
--

-- | A DAU is a discrete component in a larger system. It has an ID, several
--   named ports, and a fixed monetary cost. The ports of a DAU may be either
--   grouped or ungrouped; the type parameter 'p' is used to range over these
--   two possibilities.
data Dau p = MkDau {
     dauID   :: Name  -- ^ globally unique ID of this DAU
   , ports   :: p     -- ^ named ports
   , monCost :: Int   -- ^ fixed monetary cost of the DAU
} deriving (Typeable,Generic,Eq,Show)


-- ** Ports and attributes

-- | A list of ports.
type Ports a = [Port a]

-- | A list of port groups.
type PortGroups a = [PortGroup a]

-- | Named attributes associated with a port. For a fully configured DAU,
--   the values of the port attributes will be of type 'AttrVal', while for an
--   unconfigured DAU, the values of port attributes will be 'Constraint',
--   which captures the range of possible values the port can take on.
newtype PortAttrs a = MkPortAttrs (Env Name a)
  deriving (Typeable,Generic,Eq,Show,Functor)

-- | Convert a set of port attributes to a list of name-value pairs.
portAttrsToList :: PortAttrs a -> [(Name,a)]
portAttrsToList (MkPortAttrs m) = envToList m

-- | Convert a list of name-value pairs to a set of port attributes.
portAttrsFromList :: [(Name,a)] -> PortAttrs a
portAttrsFromList = MkPortAttrs . envFromList

-- | A port is a named connection to a DAU and its associated attributes.
data Port a = MkPort {
     portID    :: Name          -- ^ unique ID
   , portFunc  :: Name          -- ^ core functionality of this port
   , portAttrs :: PortAttrs a   -- ^ named port attributes
} deriving (Typeable,Generic,Eq,Show)

-- | A port group is a set of ports with identical attributes.
data PortGroup a = MkPortGroup {
     groupIDs   :: [Name]       -- ^ list of port IDs in this group
   , groupFunc  :: Name         -- ^ functionality of the ports in this group
   , groupAttrs :: PortAttrs a  -- ^ attributes shared among ports in the group
   , groupSize  :: Int          -- ^ number of ports in the group
} deriving (Typeable,Generic,Eq,Show)

-- | A constraint on a port attribute in an unconfigured DAU.
data Constraint
   = Exactly PVal
   | OneOf [PVal]
   | Range Int Int
   | Sync [PortAttrs Constraint]
  deriving (Typeable,Generic,Eq,Show)

-- | A value of a port attribute in a configured DAU.
data AttrVal
   = Leaf PVal
   | Node (PortAttrs AttrVal)
  deriving (Typeable,Generic,Eq,Show)


--
-- * Attribute rules
--

-- | A rule refines the check associated with a particular attribute. There
--   are two kinds of rules: Equations define derived attributes whose values
--   are determined by an arithmetic expression over other attribute values.
--   Compatibility rules describe a list of values that are considered
--   compatible with a given value of a given attribute.
data Rule
   = Compatible [PVal]
   | Equation Expr
  deriving (Typeable,Generic,Eq,Show)

-- | A set of rules that apply to a given attribute-value pair.
newtype Rules = MkRules (Env (Name,PVal) Rule)
  deriving (Typeable,Generic,Eq,Show)


--
-- * Inventories and provisions
--

-- | A DAU inventory is a list of available DAUs, sorted by monetary cost,
--   where the ports in each DAU have been organized into groups.
--   -- TODO: Make this a set so ordering of JSON file doesn't matter
type Inventory = [Dau (PortGroups Constraint)]

-- | A provision associates a port group with its DAU and group index. This is
--   information that can be derived directly from the DAU description, but
--   is used in the provision map for quickly looking up relevant port groups
--   by functionality.
data Provision = MkProvision {
     provDau   :: Name                  -- ^ DAU this provision belongs to
   , provIdx   :: Int                   -- ^ group index of this provision
   , provGroup :: PortGroup Constraint  -- ^ the provided port groups
} deriving (Typeable,Generic,Eq,Show)

-- | Associates port functionalities with port groups from provided DAUs.
type Provisions = Env Name [Provision]

-- | The number of ports providing each functionality within an inventory.
type PortCount = Env Name Int


--
-- * Requests and Responses
--

-- | Set the current DAU inventory.
data SetInventory = MkSetInventory {
     invDaus :: [Dau (Ports Constraint)]
} deriving (Typeable,Generic,Eq,Show)

-- | An adaptation request contains the relevant DAUs that may need to be
--   replaced.
data Request = MkRequest {
     reqDaus :: [RequestDau]
} deriving (Typeable,Generic,Eq,Show)

-- | A DAU in an adaptation request, which may be flagged for replacement,
--   whose ports are constrained according to the requirements of the
--   system it lives in.
data RequestDau = MkRequestDau {
     replace :: Bool
   , reqDau  :: Dau (Ports Constraint)
} deriving (Typeable,Generic,Eq,Show)

-- | An adaptation response consists of configured DAUs.
data Response = MkResponse {
     resDaus :: [ResponseDau]
} deriving (Typeable,Generic,Eq,Show)

-- | A port in an adaptation response, which replaces another named port.
data ResponsePort = MkResponsePort {
     oldPort :: Name
   , resPort :: Port AttrVal
} deriving (Typeable,Generic,Eq,Show)

-- | A DAU in an adaptation response, which replaces one or more DAUs in the
--   corresponding request, whose ports are configured to single values.
data ResponseDau = MkResponseDau {
     oldDaus :: [Name]
   , resDau  :: Dau [ResponsePort]
} deriving (Typeable,Generic,Eq,Show)


--
-- * Metrics
--

-- | A set of metrics to help characterize the size of the search space.
data Metrics = MkMetrics {
     numReqDaus   :: Int  -- ^ required DAUs (i.e. DAUs to replace)
   , numReqPorts  :: Int  -- ^ ports in required DAUs
   , numReqGroups :: Int  -- ^ unique port gropus in required DAUs
   , numInvDaus   :: Int  -- ^ DAUs in the inventory
   , numInvs      :: Int  -- ^ sub-inventories considered
   , numIgnored   :: Int  -- ^ sub-inventories filtered out before analysis
   , numExplored  :: Int  -- ^ sub-inventories explored by variational analysis
   , numExpPorts  :: Int  -- ^ total number of ports in explored sub-inventories
   , numExpGroups :: Int  -- ^ total number of port groups in explored sub-inventories
   , numCfgDims   :: Int  -- ^ total number of configuration dimensions in explored sub-inventories
   , numUseDims   :: Int  -- ^ total number of provision-to-requirement use dimensions in explored sub-inventories
} deriving (Typeable,Generic,Eq,Show)
