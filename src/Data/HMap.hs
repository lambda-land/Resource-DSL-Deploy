{-# LANGUAGE
      DeriveDataTypeable,
      DeriveGeneric,
      FlexibleContexts,
      GeneralizedNewtypeDeriving
  #-}

-- | A hierarchical map data structure.
module Data.HMap where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Control.Monad (foldM)
import Control.Monad.Except (MonadError,catchError,throwError)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set


--
-- * Hierarchical Map
--

-- ** Type

-- | An entry in a hierarchical map. Either an internal node or a leaf.
type Entry k v = Either (HMap k v) v

-- | A hierarchical map.
newtype HMap k v = HMap (Map k (Entry k v))
  deriving (Data,Eq,Generic,Monoid,Read,Show,Typeable)

instance Functor (HMap k) where
  fmap = mapLeaves


-- ** Errors

-- | Kinds of errors that can occur in HMap operations.
data ErrorKind
     = EmptyPath
     | EntryIsNotNode
     | EntryIsNotLeaf
     | EntryNotFound
     | CannotMergeEntries
  deriving (Data,Eq,Generic,Read,Show,Typeable)

-- | An error from an HMap operation, along with the path where it occurred.
data Error k = Error {
     errorPath :: [k],
     errorKind :: ErrorKind
} deriving (Data,Eq,Generic,Read,Show,Typeable)

-- | Throw an empty path error.
errorEmpty :: MonadError (Error k) m => m a
errorEmpty = throwError (Error [] EmptyPath)

-- | Throw an error at a particular key.
errorAt :: MonadError (Error k) m => k -> ErrorKind -> m a
errorAt k e = throwError (Error [k] e)

-- | Replace the path of any thrown errors.
onPath :: (Ord k, MonadError (Error k) m) => [k] -> m a -> m a
onPath p x = catchError x (\(Error _ e) -> throwError (Error p e))


-- ** Converting Entries

-- | Convert an entry to an internal node or throw an error if it's a leaf.
assumeNode :: (Ord k, MonadError (Error k) m)
  => k -> Entry k v -> m (HMap k v)
assumeNode _ (Left h) = return h
assumeNode k _        = errorAt k EntryIsNotNode

-- | Convert an entry to a leaf or throw an error if it's an internal node.
assumeLeaf :: (Ord k, MonadError (Error k) m)
  => k -> Entry k v -> m v
assumeLeaf _ (Right v) = return v
assumeLeaf k _         = errorAt k EntryIsNotLeaf


-- ** Construction

-- | An empty hierarchical map.
empty :: HMap k v
empty = HMap Map.empty

-- | A singleton hierarchical map.
singleton :: k -> v -> HMap k v
singleton k v = HMap (Map.singleton k (Right v))


-- ** Query

-- | Is the map empty?
null :: HMap k v -> Bool
null (HMap m) = Map.null m

-- | The number of leaf elements in the map.
size :: HMap k v -> Int
size = undefined

-- | Is this key in the map?
member :: Ord k => k -> HMap k v -> Bool
member k (HMap m) = Map.member k m

-- | Lookup an entry in the map.
lookupEntry :: (Ord k, MonadError (Error k) m)
  => k -> HMap k v -> m (Entry k v)
lookupEntry k (HMap m) = maybe err return (Map.lookup k m)
  where err = errorAt k EntryNotFound

-- | Lookup an internal node in the map.
lookupNode :: (Ord k, MonadError (Error k) m)
  => k -> HMap k v -> m (HMap k v)
lookupNode k h = lookupEntry k h >>= assumeNode k

-- | Lookup a leaf in the map.
lookupLeaf :: (Ord k, MonadError (Error k) m)
  => k -> HMap k v -> m v
lookupLeaf k h = lookupEntry k h >>= assumeLeaf k

-- | Lookup an internal node in the map, or return a new empty node if no
--   entry exists.
lookupOrCreateNode :: (Ord k, MonadError (Error k) m)
  => k -> HMap k v -> m (HMap k v)
lookupOrCreateNode k (HMap m) = case Map.lookup k m of
    Just e  -> assumeNode k e
    Nothing -> return empty

-- | Apply a query at the end of a path of keys.
queryPath :: (Ord k, MonadError (Error k) m)
  => (k -> HMap k v -> m a) -> [k] -> HMap k v -> m a
queryPath f p h = onPath p (go p h)
  where
    go []     _ = errorEmpty
    go [k]    h = f k h
    go (k:ks) h = lookupNode k h >>= go ks

-- | Apply a query at the end of a path of keys, or return a default value on
--   an error.
queryPathWithDefault :: (Ord k, MonadError (Error k) m)
  => (k -> HMap k v -> m a) -> a -> [k] -> HMap k v -> m a
queryPathWithDefault f d p h = catchError (queryPath f p h) (const (return d))


-- ** Modification

-- | Update the entry associated with a key in the map.
--   If the key is not present, return the map unchanged.
adjustEntry :: Ord k => (Entry k v -> Entry k v) -> k -> HMap k v -> HMap k v
adjustEntry f k (HMap m) = HMap (Map.adjust f k m)

-- | Delete the entry associated with a key in the map.
--   If the key is not present, return the map unchanged.
deleteEntry :: Ord k => k -> HMap k v -> HMap k v
deleteEntry k (HMap m) = HMap (Map.delete k m)

-- | Insert a new key and entry into the map.
--   If the key is already in the map, the entry is replaced.
insertEntry :: Ord k => k -> Entry k v -> HMap k v -> HMap k v
insertEntry k e (HMap m) = HMap (Map.insert k e m)

-- | Insert a new key and node into the map.
--   If the key is already in the map, the entry is replaced.
insertNode :: Ord k => k -> HMap k v -> HMap k v -> HMap k v
insertNode k = insertEntry k . Left

-- | Insert a new key and leaf into the map.
--   If the key is already in the map, the entry is replaced.
insertLeaf :: Ord k => k -> v -> HMap k v -> HMap k v
insertLeaf k = insertEntry k . Right

-- | Apply a modification at the end of a path of keys, creating the path
--   along the way if needed.
modifyPath :: (Ord k, MonadError (Error k) m)
  => (k -> HMap k v -> HMap k v) -> [k] -> HMap k v -> m (HMap k v)
modifyPath f p h = onPath p (go p h)
  where
    go []     _ = errorEmpty
    go [k]    h = return (f k h)
    go (k:ks) h = do
      old <- lookupOrCreateNode k h
      new <- go ks old
      return (insertNode k new h)


-- ** Combine

-- | Recursive union of two hierarchical maps. Leaves are merged with the
--   given function. Throws an error if forced to merge a node and leaf.
unionWith :: (Ord k, MonadError (Error k) m)
  => (v -> v -> m v) -> HMap k v -> HMap k v -> m (HMap k v)
unionWith f (HMap m1) (HMap m2) = do 
    let todo = Map.keys (Map.intersection m1 m2)
    let rest = Map.difference m1 m2 `Map.union` Map.difference m2 m1
    m <- foldM merge Map.empty todo
    return (HMap (m `Map.union` rest))
  where
    merge m k = case (m1 Map.! k, m2 Map.! k) of
      (Left h1, Left h2) -> do
        h <- unionWith f h1 h2
        return (Map.insert k (Left h) m)
      (Right v1, Right v2) -> do
        v <- f v1 v2
        return (Map.insert k (Right v) m)
      _ -> errorAt k CannotMergeEntries


-- ** Traversal

-- | Map a function over all of the values at the leaves.
mapLeaves :: (a -> b) -> HMap k a -> HMap k b
mapLeaves f (HMap m) = HMap (fmap (either (Left . fmap f) (Right . f)) m)
