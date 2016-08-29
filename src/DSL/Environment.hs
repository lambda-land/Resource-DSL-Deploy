{-# LANGUAGE
      DeriveDataTypeable,
      DeriveGeneric,
      FlexibleContexts,
      GeneralizedNewtypeDeriving
  #-}

module DSL.Environment where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Control.Monad (foldM)
import Control.Monad.Except (MonadError,catchError,throwError)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set


--
-- * Generic Environments
--

-- ** Type

-- | Names.
type Name = String

-- | Environments map names to values.
type Env v = Map Name v


-- ** Errors

-- | Error thrown when a name is not found in the environment.
newtype NotFound = NotFound Name
  deriving (Data,Eq,Generic,Read,Show,Typeable)


-- ** Construction

-- | Empty enivornment.
envEmpty :: Env v
envEmpty = Map.empty

-- | Smart constructor for environments.
envFromList :: [(Name,v)] -> Env v
envFromList = Map.fromList


-- ** Operations

-- | Check whether an environment contains a particular name.
envHas :: Name -> Env v -> Bool
envHas = Map.member

-- | Extend an environment with a new name binding.
envExtend :: Name -> v -> Env v -> Env v
envExtend = Map.insert

-- | Lookup a binding in an environment.
envLookup :: MonadError NotFound m => Name -> Env v -> m v
envLookup k m = maybe notFound return (Map.lookup k m)
  where notFound = throwError (NotFound k)

-- | Assume a binding is found, failing dynamically otherwise.
assumeFound :: Either NotFound v -> v
assumeFound (Right a)  = a
assumeFound (Left err) = error msg
  where msg = "assumeFound: bad assumption: " ++ show err


--
-- * Hierarchical Environments
--

-- ** Type

-- | A path through a hierarchical environment.
type Path = [Name]

-- | An entry in a hierarchical environment. Either an internal node or a leaf.
type Entry v = Either (HEnv v) v

-- | A hierarchical environment.
newtype HEnv v = HEnv (Env (Entry v))
  deriving (Data,Eq,Generic,Monoid,Read,Show,Typeable)

instance Functor HEnv where
  fmap = mapLeaves


-- ** Errors

-- | Kinds of errors that can occur in HEnv operations.
data HEnvErrorKind
     = EmptyPath
     | EntryIsNotNode
     | EntryIsNotLeaf
     | EntryNotFound
     | CannotMergeEntries
  deriving (Data,Eq,Generic,Read,Show,Typeable)

-- | An error from an HEnv operation, along with the path where it occurred.
data HEnvError = HEnvError {
     errorPath :: Path,
     errorKind :: HEnvErrorKind
} deriving (Data,Eq,Generic,Read,Show,Typeable)

-- | Throw an empty path error.
henvEmptyPath :: MonadError HEnvError m => m a
henvEmptyPath = throwError (HEnvError [] EmptyPath)

-- | Throw an error associated with a particular name.
henvError :: MonadError HEnvError m => Name -> HEnvErrorKind -> m a
henvError k e = throwError (HEnvError [k] e)

-- | Replace the path of any thrown errors.
onPath :: MonadError HEnvError m => Path -> m a -> m a
onPath p x = catchError x (\(HEnvError _ e) -> throwError (HEnvError p e))


-- ** Construction

-- | Empty hierarchical enivornment.
henvEmpty :: HEnv v
henvEmpty = HEnv Map.empty

-- | Extend a hierarchical environment with a new binding.
henvExtend :: MonadError HEnvError m => Path -> v -> HEnv v -> m (HEnv v)
henvExtend p v = modifyPath (\k -> insertLeafHere k v) p


-- ** Operations

-- | Is there an entry at this path in the hierarchical map?
henvHas :: MonadError HEnvError m => Path -> HEnv v -> m Bool
henvHas = queryPathWithDefault (\k h -> return (hasHere k h)) False

-- | Lookup a value in a hierarchical environment using a path.
henvLookup :: MonadError HEnvError m => Path -> HEnv v -> m v
henvLookup = queryPath lookupLeafHere


-- ** Converting Entries

-- | Convert an entry to an internal node or throw an error if it's a leaf.
assumeNode :: MonadError HEnvError m => Name -> Entry v -> m (HEnv v)
assumeNode _ (Left h) = return h
assumeNode k _        = henvError k EntryIsNotNode

-- | Convert an entry to a leaf or throw an error if it's an internal node.
assumeLeaf :: MonadError HEnvError m => Name -> Entry v -> m v
assumeLeaf _ (Right v) = return v
assumeLeaf k _         = henvError k EntryIsNotLeaf


-- ** Query

-- | Is there an entry with the given name at this node?
hasHere :: Name -> HEnv v -> Bool
hasHere k (HEnv m) = Map.member k m

-- | Lookup an entry at this node in the HEnv.
lookupEntryHere :: MonadError HEnvError m => Name -> HEnv v -> m (Entry v)
lookupEntryHere k (HEnv m) = maybe err return (Map.lookup k m)
  where err = henvError k EntryNotFound

-- | Lookup an internal node at this node in the HEnv.
lookupNodeHere :: MonadError HEnvError m => Name -> HEnv v -> m (HEnv v)
lookupNodeHere k h = lookupEntryHere k h >>= assumeNode k

-- | Lookup a leaf at this node in the HEnv.
lookupLeafHere :: MonadError HEnvError m => Name -> HEnv v -> m v
lookupLeafHere k h = lookupEntryHere k h >>= assumeLeaf k

-- | Lookup an internal node at this node in the HEnv,
--   or return an empty node if no entry exists.
lookupOrCreateNodeHere :: MonadError HEnvError m => Name -> HEnv v -> m (HEnv v)
lookupOrCreateNodeHere k (HEnv m) = case Map.lookup k m of
    Just e  -> assumeNode k e
    Nothing -> return henvEmpty

-- | Apply a query at the end of a path.
queryPath :: MonadError HEnvError m
  => (Name -> HEnv v -> m a) -> Path -> HEnv v -> m a
queryPath f p h = onPath p (go p h)
  where
    go []     _ = henvEmptyPath
    go [k]    h = f k h
    go (k:ks) h = lookupNodeHere k h >>= go ks

-- | Apply a query at the end of a path, or return a default value on an error.
queryPathWithDefault :: MonadError HEnvError m
  => (Name -> HEnv v -> m a) -> a -> Path -> HEnv v -> m a
queryPathWithDefault f d p h = catchError (queryPath f p h) (const (return d))


-- ** Modification

-- | Update the entry associated with a name at this node in the map.
--   If the name is not present, return the map unchanged.
adjustEntryHere :: (Entry v -> Entry v) -> Name -> HEnv v -> HEnv v
adjustEntryHere f k (HEnv m) = HEnv (Map.adjust f k m)

-- | Delete the entry associated with a name at this node in the map.
--   If the name is not present, return the map unchanged.
deleteEntryHere :: Name -> HEnv v -> HEnv v
deleteEntryHere k (HEnv m) = HEnv (Map.delete k m)

-- | Insert a new name and entry into this node in the map.
--   If the name is already in the map, the entry is replaced.
insertEntryHere :: Name -> Entry v -> HEnv v -> HEnv v
insertEntryHere k e (HEnv m) = HEnv (Map.insert k e m)

-- | Insert a new name and node into this node in the map.
--   If the name is already in the map, the entry is replaced.
insertNodeHere :: Name -> HEnv v -> HEnv v -> HEnv v
insertNodeHere k = insertEntryHere k . Left

-- | Insert a new key and leaf into the map.
--   If the key is already in the map, the entry is replaced.
insertLeafHere :: Name -> v -> HEnv v -> HEnv v
insertLeafHere k = insertEntryHere k . Right

-- | Apply a modification at the end of a path of keys, creating the path
--   along the way if needed.
modifyPath :: MonadError HEnvError m
  => (Name -> HEnv v -> HEnv v) -> Path -> HEnv v -> m (HEnv v)
modifyPath f p h = onPath p (go p h)
  where
    go []     _ = henvEmptyPath
    go [k]    h = return (f k h)
    go (k:ks) h = do
      old <- lookupOrCreateNodeHere k h
      new <- go ks old
      return (insertNodeHere k new h)


-- ** Combine

-- | Recursive union of two hierarchical maps. Leaves are merged with the
--   given function. Throws an error if forced to merge a node and leaf.
henvUnionWith :: MonadError HEnvError m
  => (v -> v -> m v) -> HEnv v -> HEnv v -> m (HEnv v)
henvUnionWith f (HEnv m1) (HEnv m2) = do 
    let todo = Map.keys (Map.intersection m1 m2)
    let rest = Map.difference m1 m2 `Map.union` Map.difference m2 m1
    m <- foldM merge Map.empty todo
    return (HEnv (m `Map.union` rest))
  where
    merge m k = case (m1 Map.! k, m2 Map.! k) of
      (Left h1, Left h2) -> do
        h <- henvUnionWith f h1 h2
        return (Map.insert k (Left h) m)
      (Right v1, Right v2) -> do
        v <- f v1 v2
        return (Map.insert k (Right v) m)
      _ -> henvError k CannotMergeEntries


-- ** Traversal

-- | Map a function over all of the values at the leaves.
mapLeaves :: (a -> b) -> HEnv a -> HEnv b
mapLeaves f (HEnv m) = HEnv (fmap (either (Left . fmap f) (Right . f)) m)
