module DSL.Env where

import Control.Monad.Except (throwError)
import Control.Monad.Reader
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.SBV


--
-- * Environments
--

-- | Names.
type Name = String

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
envLookup :: Name -> Env b -> Maybe b
envLookup = Map.lookup

-- | Lookup a binding in an environment or fail dynamically if it's not there.
envLookup' :: Name -> Env b -> b
envLookup' v m | Just b <- Map.lookup v m = b
               | otherwise = error $ "Name is not in environment: " ++ v

-- | Lookup the value associated with a name and remove it from the environment.
envExtract :: Name -> Env a -> Maybe (a, Env a)
envExtract v r = ma >>= \a -> Just (a,r')
  where (ma,r') = Map.updateLookupWithKey (\_ _ -> Nothing) v r
