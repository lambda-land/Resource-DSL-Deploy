module DSL.Env where

import Control.Monad.Reader
import Control.Monad.State

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.SBV


--
-- * Environments
--

-- | Variable names.
type Var = String

-- | Variable environment.
type Env a = Map Var a

-- | Lookup a binding in an environment or fail dynamically if it's not there.
envLookup :: Var -> Env b -> b
envLookup v m | Just b <- Map.lookup v m = b
              | otherwise = error $ "Variable not in environment: " ++ v

-- | Generate an environment with fresh symbolic values for each variable.
symEnv :: (Var -> Symbolic b) -> Set Var -> Symbolic (Env b)
symEnv f s = fmap (Map.fromList . zip vs) (mapM f vs)
  where vs = Set.toList s


--
-- * Linear Environment Monad
--

-- | The linear environment monad tracks two separate environments: one for
--   non-linear assumptions, and one for linear assumptions. It's implemented
--   by a monad transformer stack, structured like so:
--    * Reader: for non-linear assumptions
--    * State: for linear assumptions
--    * Either String: for error reporting
type EnvM b = ReaderT (Env b) (StateT (Env b) (Either String))

-- | Lookup a non-linear assumption.
lookRef :: Var -> EnvM b b
lookRef v = do
    m <- ask
    case Map.lookup v m of
      Just t -> return t
      _      -> fail ("unbound non-linear variable: " ++ v)

-- | Lookup and consume a linear assumption.
lookUse :: Var -> EnvM b b
lookUse v = do
    m <- get
    case Map.lookup v m of
      Just t -> put (Map.delete v m) >> return t
      _      -> fail ("unavailable linear variable: " ++ v)

-- | Push a linear assumption onto the environment, then run a computation
--   and check to see whether the assumption was consumed. If so, return the 
--   result. Otherwise, fail.
runWith :: Var -> b -> EnvM b a -> EnvM b a
runWith v b ma = do
    old <- gets (Map.lookup v)
    modify (Map.insert v b)
    result <- ma
    unused <- gets (Map.member v)
    when unused (fail ("unused linear variable: " ++ v))
    case old of
      Just t -> modify (Map.insert v b)
      _      -> return ()
    return result
