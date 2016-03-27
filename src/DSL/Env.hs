module DSL.Env where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.SBV


--
-- * Environments
--

-- | Variable names.
type Var = String

-- | Variable environment.
type Env a = Map Var a

-- | Lookup a binding in an environment or fail dynamically if it's not there.
envLookup :: Var -> Env a -> a
envLookup v m | Just a <- Map.lookup v m = a
              | otherwise = error $ "Variable not in environment: " ++ v

-- | Generate an environment with fresh symbolic values for each variable.
symEnv :: (Var -> Symbolic a) -> Set Var -> Symbolic (Env a)
symEnv f s = fmap (Map.fromList . zip vs) (mapM f vs)
  where vs = Set.toList s
