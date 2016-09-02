module DSL.Environment where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Control.Monad (foldM)
import Control.Monad.Catch (Exception,MonadCatch,MonadThrow,catch,throwM)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set


--
-- * Generic Environments
--

-- ** Types

-- | Names.
type Name = String

-- | Variable names.
type Var = Name

-- | A path through a hierarchical environment.
type Path = [Name]

-- | An environment is a map from keys to values.
type Env k v = Map k v


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
envEmpty = Map.empty

-- | Construct an environment from an association list.
envFromList :: Ord k => [(k,v)] -> Env k v
envFromList = Map.fromList


-- ** Operations

-- | Check whether an environment contains a particular name.
envHas :: Ord k => k -> Env k v -> Bool
envHas = Map.member

-- | Extend an environment with a new name binding.
envExtend :: Ord k => k -> v -> Env k v -> Env k v
envExtend = Map.insert

-- | Delete a binding in an environment.
envDelete :: Ord k => k -> Env k v -> Env k v
envDelete = Map.delete

-- | Left-biased union of two environments.
envUnion :: Ord k => Env k v -> Env k v -> Env k v
envUnion = Map.union

-- | Left-biased union of two environments.
envUnionWith :: Ord k => (v -> v -> v) -> Env k v -> Env k v -> Env k v
envUnionWith = Map.unionWith

-- | Lookup a binding in an environment.
envLookup :: (Ord k, Show k, Typeable k, MonadThrow m) => k -> Env k v -> m v
envLookup k m = maybe notFound return (Map.lookup k m)
  where notFound = throwM (NotFound k)

-- | Apply a result-less monadic action to all key-value pairs.
envMapM_ :: Monad m => (k -> v -> m ()) -> Env k v -> m ()
envMapM_ f m = mapM_ (uncurry f) (Map.toAscList m)
