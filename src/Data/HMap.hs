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
import Control.Monad.Except (MonadError,throwError)

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

-- | Convert an entry to an internal node or throw an error if it's a leaf.
assumeNode :: MonadError String m => Entry k v -> m (HMap k v)
assumeNode (Left h) = return h
assumeNode _        = throwError "assumeNode: not a node"

-- | Convert an entry to a leaf or throw an error if it's an internal node.
assumeLeaf :: MonadError String m => Entry k v -> m v
assumeLeaf (Right v) = return v
assumeLeaf _         = throwError "assumeLeaf: not a leaf"


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
lookupEntry :: (Ord k, MonadError String m)
            => k -> HMap k v -> m (Entry k v)
lookupEntry k (HMap m) = maybe err return (Map.lookup k m)
  where err = throwError "lookupEntry: no entry for key in map"

-- | Lookup an internal node in the map.
lookupNode :: (Ord k, MonadError String m)
           => k -> HMap k v -> m (HMap k v)
lookupNode k h = lookupEntry k h >>= assumeNode

-- | Lookup a leaf in the map.
lookupLeaf :: (Ord k, MonadError String m)
           => k -> HMap k v -> m v
lookupLeaf k h = lookupEntry k h >>= assumeLeaf

-- | Apply a query at the end of a path of keys.
queryPath :: (Ord k, MonadError String m)
            => (k -> HMap k v -> m a) -> [k] -> HMap k v -> m a
queryPath _ []     _ = throwError "queryAtPath: empty path"
queryPath f [k]    h = f k h
queryPath f (k:ks) h = lookupNode k h >>= queryPath f ks


-- ** Modification

-- | Update an entry in the map.
updateEntry :: (Ord k, MonadError String m)
            => (Entry k v -> Maybe (Entry k v)) -> k -> HMap k v -> m (HMap k v)
updateEntry = undefined

-- | Insert an entry into the map.
insertEntry :: (Ord k, MonadError String m)
            => k -> Entry k v -> HMap k v -> m (HMap k v)
insertEntry k e = updateEntry (\_ -> Just e) k

-- | Delete an entry in the map.
deleteEntry :: (Ord k, MonadError String m)
            => k -> HMap k v -> m (HMap k v)
deleteEntry = updateEntry (\_ -> Nothing)


-- ** Traversal

-- | Map a function over all of the values at the leaves.
mapLeaves :: (a -> b) -> HMap k a -> HMap k b
mapLeaves f (HMap m) = HMap (fmap (either (Left . fmap f) (Right . f)) m)
