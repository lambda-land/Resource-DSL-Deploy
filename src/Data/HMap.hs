{-# LANGUAGE
      DeriveDataTypeable,
      DeriveGeneric,
      FlexibleContexts,
      GeneralizedNewtypeDeriving
  #-}

-- | A hierarchical map data structure.
module Data.HMap where

import Data.Data    (Data,Typeable)
import GHC.Generics (Generic)

import Control.Monad        (foldM)
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

-- | An error that can occur in HMap operations.
data HMapError k
    = EmptyPath
    | EntryIsNotNode
    | EntryIsNotLeaf
    | EntryNotFound
    | CannotMergeEntries
    | OnPath k (HMapError k)
  deriving (Data,Eq,Generic,Read,Show,Typeable)

-- | Run a computation that may throw an HMapError, extending it's path if
--   an error occurs.
onPath :: MonadError (HMapError k) m => k -> m a -> m a
onPath k x = catchError x (\e -> throwError (OnPath k e))


-- ** Converting Entries

-- | Convert an entry to an internal node or throw an error if it's a leaf.
assumeNode :: MonadError (HMapError k) m
  => Entry k v -> m (HMap k v)
assumeNode (Left h) = return h
assumeNode _        = throwError EntryIsNotNode

-- | Convert an entry to a leaf or throw an error if it's an internal node.
assumeLeaf :: MonadError (HMapError k) m
  => Entry k v -> m v
assumeLeaf (Right v) = return v
assumeLeaf _         = throwError EntryIsNotLeaf


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

-- | Lookup an entry in the map.
lookupEntry :: (Ord k, MonadError (HMapError k) m)
  => k -> HMap k v -> m (Entry k v)
lookupEntry k (HMap m) = maybe err return (Map.lookup k m)
  where err = throwError EntryNotFound

-- | Lookup an internal node in the map.
lookupNode :: (Ord k, MonadError (HMapError k) m)
  => k -> HMap k v -> m (HMap k v)
lookupNode k h = lookupEntry k h >>= assumeNode

-- | Lookup a leaf in the map.
lookupLeaf :: (Ord k, MonadError (HMapError k) m)
  => k -> HMap k v -> m v
lookupLeaf k h = lookupEntry k h >>= assumeLeaf

-- | Lookup an internal node in the map, or return a new empty node if no
--   entry exists.
lookupOrCreateNode :: (Ord k, MonadError (HMapError k) m)
  => k -> HMap k v -> m (HMap k v)
lookupOrCreateNode k (HMap m) = case Map.lookup k m of
    Just e  -> assumeNode e
    Nothing -> return empty

-- | Apply a query at the end of a path of keys.
queryPath :: (Ord k, MonadError (HMapError k) m)
  => (k -> HMap k v -> m a) -> [k] -> HMap k v -> m a
queryPath _ []     _ = throwError EmptyPath
queryPath f [k]    h = f k h
queryPath f (k:ks) h = onPath k (lookupNode k h >>= queryPath f ks)


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
modifyPath :: (Ord k, MonadError (HMapError k) m)
            => (k -> HMap k v -> HMap k v) -> [k] -> HMap k v -> m (HMap k v)
modifyPath _ []     _ = throwError EmptyPath
modifyPath f [k]    h = return (f k h)
modifyPath f (k:ks) h = onPath k $ do
    old <- lookupOrCreateNode k h
    new <- modifyPath f ks old
    return (insertNode k new h)


-- ** Combine

-- | Recursive union of two hierarchical maps. Leaves are merged with the
--   given function. Throws an error if forced to merge a node and leaf.
unionWith :: (Ord k, MonadError (HMapError k) m)
          => (v -> v -> m v) -> HMap k v -> HMap k v -> m (HMap k v)
unionWith f (HMap m1) (HMap m2) = do 
    let todo = Map.keys (Map.intersection m1 m2)
    let rest = Map.difference m1 m2 `Map.union` Map.difference m2 m1
    m <- foldM merge Map.empty todo
    return (HMap (m `Map.union` rest))
  where
    merge m k = onPath k $
      case (m1 Map.! k, m2 Map.! k) of
        (Left h1, Left h2) -> do
          h <- unionWith f h1 h2
          return (Map.insert k (Left h) m)
        (Right v1, Right v2) -> do
          v <- f v1 v2
          return (Map.insert k (Right v) m)
        _ -> throwError CannotMergeEntries


-- ** Traversal

-- | Map a function over all of the values at the leaves.
mapLeaves :: (a -> b) -> HMap k a -> HMap k b
mapLeaves f (HMap m) = HMap (fmap (either (Left . fmap f) (Right . f)) m)
