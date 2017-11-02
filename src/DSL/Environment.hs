module DSL.Environment where

import Data.Composition ((.:))
import Control.Monad.Except

import Data.Typeable
import Data.List (union)

import qualified Data.Map.Strict as Map

import DSL.Types


--
-- * Generic Environments
--

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
envSingle :: k -> v -> Env k v
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

-- Merge duplicate dictionary entries. For now, merges profiles w/ profiles
-- and models w/ models, otherwise throws an error.
instance MergeDup Entry where
  mergeDup (ProEntry l) (ProEntry r) = ProEntry (mergeDup l r)
  mergeDup (ModEntry l) (ModEntry r) = ModEntry (mergeDup l r)
  mergeDup _ _ = error "mergeDup (Entry): cannot merge profile and model"

-- Throw an error if we attempt to merge two primitive values.
instance MergeDup Value where
  mergeDup _ _ = error "mergeDup (Value): attempted to merge duplicate entries"

-- | Compose two resource profiles. Merges parameters by name.
composeProfiles :: Profile -> Profile -> Profile
composeProfiles (Profile ps1 h1) (Profile ps2 h2) =
    Profile (union ps1 ps2) (envUnionWith (++) h1 h2)

instance MergeDup Profile where
  mergeDup = composeProfiles

-- | Compose two models by sequencing the statements in their bodies.
--   Merges parameters by name.
composeModels :: Model -> Model -> Model
composeModels (Model ps1 b1) (Model ps2 b2) =
    Model (union ps1 ps2) (b1 ++ b2)

instance MergeDup Model where
  mergeDup = composeModels

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
envLookup :: (MonadError Error m, Ord k, Show k, Typeable k) => k -> Env k v -> m v
envLookup k (Env m) = (maybe notFound return . Map.lookup k) m
  where notFound = throwError (EnvE $ NotFound k (Map.keys m))

-- | Apply a result-less monadic action to all key-value pairs.
envMapM_ :: Monad m => (k -> v -> m ()) -> Env k v -> m ()
envMapM_ f = mapM_ (uncurry f) . Map.toAscList . envAsMap
