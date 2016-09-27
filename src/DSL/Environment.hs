module DSL.Environment where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Data.Composition ((.:))
import Control.Monad (foldM)
import Control.Monad.Catch (Exception,MonadCatch,MonadThrow,catch,throwM)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set


--
-- * Generic Environments
--

-- ** Type

-- | An environment is a map from keys to values.
newtype Env k v = Env { envAsMap :: Map k v }
  deriving (Data,Eq,Generic,Read,Show,Typeable)

-- | Apply a function to the map that implements this environment.
envOnMap :: (Map a b -> Map c d) -> Env a b -> Env c d
envOnMap f (Env m) = Env (f m)


-- ** Errors

-- | Error thrown when a name is not found in the environment.
newtype NotFound k = NotFound k
  deriving (Data,Eq,Generic,Read,Show,Typeable)

instance (Show k, Typeable k) => Exception (NotFound k)

-- | Assume an operation succeeds, failing dynamically otherwise.
assumeSuccess :: Show e => Either e v -> v
assumeSuccess (Right a)  = a
assumeSuccess (Left err) = error msg
  where msg = "assumeSuccess: bad assumption: " ++ show err


-- ** Construction

-- | Empty enivornment.
envEmpty :: Env k v
envEmpty = Env Map.empty

-- | Singleton environment.
envSingle :: Ord k => k -> v -> Env k v
envSingle = Env .: Map.singleton

-- | Construct an environment from an association list.
envFromList :: Ord k => [(k,v)] -> Env k v
envFromList = Env . Map.fromList

-- | Construct an environment from an association list, merging duplicates.
envFromListAcc :: (Ord k, MergeDup m) => [(k,m)] -> Env k m
envFromListAcc []        = envEmpty
envFromListAcc ((k,m):l) = envUnionWith mergeDup (envSingle k m) (envFromListAcc l)

-- | Type class for merging duplicate values when constructing an environment
--   from an association list.
class MergeDup v where
  mergeDup :: v -> v -> v

instance MergeDup [a] where
  mergeDup = (++)


-- ** Operations

-- | Check whether an environment contains a particular name.
envHas :: Ord k => k -> Env k v -> Bool
envHas k = Map.member k . envAsMap

-- | Extend an environment with a new name binding.
envExtend :: Ord k => k -> v -> Env k v -> Env k v
envExtend = envOnMap .: Map.insert

-- | Delete a binding in an environment.
envDelete :: Ord k => k -> Env k v -> Env k v
envDelete = envOnMap . Map.delete

-- | Left-biased union of two environments.
envUnion :: Ord k => Env k v -> Env k v -> Env k v
envUnion (Env l) (Env r) = Env (Map.union l r)

-- | Left-biased union of two environments.
envUnionWith :: Ord k => (v -> v -> v) -> Env k v -> Env k v -> Env k v
envUnionWith f (Env l) (Env r) = Env (Map.unionWith f l r)

-- | Lookup a binding in an environment.
envLookup :: (Ord k, Show k, Typeable k, MonadThrow m) => k -> Env k v -> m v
envLookup k = maybe notFound return . Map.lookup k . envAsMap
  where notFound = throwM (NotFound k)

-- | Apply a result-less monadic action to all key-value pairs.
envMapM_ :: Monad m => (k -> v -> m ()) -> Env k v -> m ()
envMapM_ f = mapM_ (uncurry f) . Map.toAscList . envAsMap

instance Functor (Env k) where
  fmap = envOnMap . fmap
