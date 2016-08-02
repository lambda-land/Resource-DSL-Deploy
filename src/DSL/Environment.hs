{-# LANGUAGE FlexibleContexts #-}

module DSL.Environment where

import Control.Monad.Except (MonadError,throwError)

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
newtype HEnv a = HEnv (Env (Either (HEnv a) a))

-- | Lookup an entry in a hierarchical environment using a path.
henvLookup :: MonadError String m => Path -> HEnv a -> m (Either (HEnv a) a)
henvLookup []    h        = return (Left h)
henvLookup [x]   (HEnv m) = envLookup x m
henvLookup (x:p) (HEnv m) = do
  r <- envLookup x m
  case r of
    Right _ -> throwError ("henvLookup: unexpected base value: " ++ x)
    Left h  -> henvLookup p h

-- | Lookup a base value in a hierarchical environment using a path.
henvLookupBase :: Path -> HEnv a -> Either String a
henvLookupBase p m = do
  r <- henvLookup p m
  case r of
    Right a -> return a
    Left _  -> throwError ("henvLookupBase: entry at path is not a base value: " ++ show p)
