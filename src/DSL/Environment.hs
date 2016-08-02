module DSL.Environment where

import Control.Monad.Except (throwError)
import Control.Monad.Reader
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.SBV


--
-- * Names
--

-- | Names.
type Name = String

-- | Variables.
type Var = Name


--
-- * Generic Environments
--

-- | Environments map names to values.
type Env a = Map Name a

-- | Smart constructor for environments.
env :: [(Name,a)] -> Env a
env = Map.fromList

-- | Construct an environment with fresh symbolic values for each variable.
symEnv :: (Name -> Symbolic b) -> Set Name -> Symbolic (Env b)
symEnv f s = fmap (Map.fromList . zip vs) (mapM f vs)
  where vs = Set.toList s


-- ** Operations

-- | Check whether an environment contains a particular name.
envHas :: Name -> Env a -> Bool
envHas = Map.member

-- | Extend an environment with a new name binding.
envExtend :: Name -> a -> Env a -> Env a
envExtend = Map.insert

-- | Lookup a binding in an environment.
envLookup :: Name -> Env a -> Either String a
envLookup x m = maybe notFound Right (Map.lookup x m)
  where notFound = Left ("envLookup: name is not in environment: " ++ x)

-- | Name not in environment error.
notFound :: Name -> Either String a
notFound x = Left ("Name is not in environment: " ++ x)

-- | Assume a binding is found, failing dynamically otherwise.
assumeFound :: Either String a -> a
assumeFound (Right a)  = a
assumeFound (Left msg) = error msg


--
-- * Hierarchical Environments
--

-- | Path through a hierarchical environment.
type Path = [Name]

-- | A hierarchical environment.
newtype HEnv a = HEnv (Env (Either (HEnv a) a))

-- | Lookup an entry in a hierarchical environment using a path.
henvLookup :: Path -> HEnv a -> Either String (Either (HEnv a) a)
henvLookup []    h        = Right (Left h)
henvLookup [x]   (HEnv m) = envLookup x m
henvLookup (x:p) (HEnv m) = case envLookup x m of
  Right (Right _) -> Left ("henvLookup: unexpected base value: " ++ x)
  Right (Left h)  -> henvLookup p h
  Left msg        -> Left msg

-- | Lookup a base value in a hierarchical environment using a path.
henvLookupBase :: Path -> HEnv a -> Either String a
henvLookupBase p m = case henvLookup p m of
  Right (Right a) -> Right a
  Right (Left _)  -> Left ("henvLookupBase: entry at path is not a base value: " ++ show p)
  Left msg        -> Left msg
