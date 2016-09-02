{-# LANGUAGE UndecidableInstances #-}

module DSL.Resource where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Control.Monad.Catch  (MonadCatch)
import Control.Monad.Reader (MonadReader,asks,local)
import Control.Monad.State  (MonadState,get,gets,modify,put)

import DSL.Environment
import DSL.Value

import {-# SOURCE #-} DSL.Effect  (Effect)
import {-# SOURCE #-} DSL.Model   (Model)
import {-# SOURCE #-} DSL.Profile (Profile)


-- 
-- * Evaluation Monad
--

-- | Variable environment.
type VarEnv = Env Var Value

-- | Resource environment.
type ResEnv = Env Path Value

-- | Dictionary of profiles and models.
type Dictionary = Env Var (Either Profile Model)

-- | Reader context for evaluation.
data Context = Ctx {
    prefix      :: Path,       -- ^ resource path prefix
    environment :: VarEnv,     -- ^ variable environment
    dictionary  :: Dictionary  -- ^ dictionary of profiles and models
} deriving (Data,Eq,Generic,Read,Show,Typeable)

-- | A monad for computations that affect a resource environment, given a
--   an evaluation context, and which may throw/catch exceptions.
class (MonadCatch m, MonadReader Context m, MonadState ResEnv m)
  => MonadEval m
instance (MonadCatch m, MonadReader Context m, MonadState ResEnv m)
  => MonadEval m

-- | Get the current resource path prefix.
getPrefix :: MonadEval m => m Path
getPrefix = asks prefix

-- | Get the current dictionary of profiles and models.
getDict :: MonadEval m => m Dictionary
getDict = asks dictionary

-- | Get the current variable environment.
getVarEnv :: MonadEval m => m VarEnv
getVarEnv = asks environment

-- | Get the current resource environment.
getResEnv :: MonadEval m => m ResEnv
getResEnv = get

-- | Query the dictionary.
queryDict :: MonadEval m => (Dictionary -> a) -> m a
queryDict f = fmap f getDict

-- | Query the variable environment.
queryVarEnv :: MonadEval m => (VarEnv -> a) -> m a
queryVarEnv f = fmap f getVarEnv

-- | Query the current resource environment.
queryResEnv :: MonadEval m => (ResEnv -> a) -> m a
queryResEnv = gets

-- | Execute a computation with an updated prefix.
withPrefix :: MonadEval m => (Path -> Path) -> m a -> m a
withPrefix f = local (\(Ctx p m d) -> Ctx (f p) m d)

-- | Execute a computation with an updated value environment.
withDict :: MonadEval m => (Dictionary -> Dictionary) -> m a -> m a
withDict f = local (\(Ctx p m d) -> Ctx p m (f d))

-- | Execute a computation with an updated value environment.
withVarEnv :: MonadEval m => (VarEnv -> VarEnv) -> m a -> m a
withVarEnv f = local (\(Ctx p m d) -> Ctx p (f m) d)

-- | Update the resource environment.
updateResEnv :: MonadEval m => (ResEnv -> ResEnv) -> m ()
updateResEnv = modify
