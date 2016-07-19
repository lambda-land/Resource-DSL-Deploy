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
envLookup :: Name -> Env a -> Maybe a
envLookup = Map.lookup

-- | Lookup a binding in an environment or fail dynamically if it's not there.
envLookup' :: Name -> Env a -> a
envLookup' v m | Just a <- envLookup v m = a
               | otherwise = error $ "Name is not in environment: " ++ v

-- | Lookup the value associated with a name and remove it from the environment.
envExtract :: Name -> Env a -> Maybe (a, Env a)
envExtract v r = ma >>= \a -> Just (a,r')
  where (ma,r') = Map.updateLookupWithKey (\_ _ -> Nothing) v r


--
-- * Hierarchical Environments
--

-- | Path through a hierarchical environment.
type Path = [Name]

-- | A hierarchical environment.
newtype HEnv a = HEnv (Env (Either (HEnv a) a))

-- | Lookup an entry in a hierarchical environment using a path.
henvLookup :: Path -> HEnv a -> Maybe (Either (HEnv a) a)
henvLookup []    _        = Nothing
henvLookup [n]   (HEnv m) = envLookup n m
henvLookup (n:p) (HEnv m) = case envLookup n m of
  Just (Left h) -> henvLookup p h
  _             -> Nothing

-- | Lookup a base value in a hierarchical environment using a path.
henvLookupBase :: Path -> HEnv a -> Maybe a
henvLookupBase p m = case henvLookup p m of
  Just (Right a) -> Just a
  _              -> Nothing
