{-# LANGUAGE
      DeriveGeneric,
      FlexibleContexts
  #-}

module DSL.Environment where

import GHC.Generics (Generic)

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
newtype HEnv a = HEnv (Env (Either (HEnv a) a))
  deriving (Eq,Generic,Show)

-- | Empty hierarchical enivornment.
henvEmpty :: HEnv a
henvEmpty = HEnv envEmpty


-- ** Operations

-- | Lookup an entry in a hierarchical environment using a path.
henvLookup :: MonadError String m => Path -> HEnv a -> m (Either (HEnv a) a)
henvLookup []    h        = return (Left h)
henvLookup [x]   (HEnv m) = envLookup x m
henvLookup (x:p) (HEnv m) = do
  r <- envLookup x m
  case r of
    Right _ -> throwError ("henvLookup: unexpected base value: " ++ x)
    Left h  -> henvLookup p h

-- | Alter an entry in a hierarchical environment. Similar to Map.alter.
henvAlter :: MonadError String m
          => Path -> (Maybe (Either (HEnv a) a) -> m (Maybe (Either (HEnv a) a)))
          -> HEnv a -> m (HEnv a)
henvAlter []    _ _        = throwError "henvAlter: empty path"
henvAlter [x]   f (HEnv m) = do
  out <- f (Map.lookup x m)
  case out of
    Nothing  -> return $ HEnv (Map.delete x m)
    Just new -> return $ HEnv (Map.insert x new m)
henvAlter (x:p) f (HEnv m) = do
  r <- envLookup x m
  case r of
    Right _ -> throwError ("henvLookup: unexpected base value: " ++ x)
    Left  h -> do h' <- henvAlter p f h
                  return $ HEnv (Map.insert x (Left h') m)

-- | Extend a hierarchical environment with a new binding.
henvExtend :: MonadError String m => Path -> a -> HEnv a -> m (HEnv a)
henvExtend p a = henvAlter p ext
  where
    ext Nothing  = return $ Just (Right a)
    ext (Just e) = throwError ("envExtend: already an entry at: " ++ show p)

-- | Lookup a base value in a hierarchical environment using a path.
henvLookupBase :: MonadError String m => Path -> HEnv a -> m a
henvLookupBase p m = do
  r <- henvLookup p m
  case r of
    Right a -> return a
    Left _  -> throwError ("henvLookupBase: entry at path is not a base value: " ++ show p)

instance Functor HEnv where
  fmap f (HEnv m) = HEnv (fmap (either (Left . fmap f) (Right . f)) m)
