{-# LANGUAGE
      DeriveGeneric,
      FlexibleContexts
  #-}

module DSL.Environment where

import GHC.Generics (Generic)

import Control.Monad.Except (MonadError,throwError)

import Data.HMap (HMap)
import qualified Data.HMap as HMap

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

-- | Empty enivornment.
envEmpty :: Env a
envEmpty = Map.empty

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
envLookup :: MonadError String m => Name -> Env a -> m a
envLookup x m = maybe notFound return (Map.lookup x m)
  where notFound = throwError ("envLookup: name is not in environment: " ++ x)

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
type HEnv a = HMap Name a

-- | Empty hierarchical enivornment.
henvEmpty :: HEnv a
henvEmpty = HMap.empty


-- ** Operations

-- | Lookup a value in a hierarchical environment using a path.
henvLookup :: MonadError String m => Path -> HEnv a -> m a
henvLookup = HMap.queryPath HMap.lookupLeaf

-- | Extend a hierarchical environment with a new binding.
henvExtend :: MonadError String m => Path -> a -> HEnv a -> m (HEnv a)
henvExtend p a = HMap.modifyPath (\k -> HMap.insertLeaf k a) p
