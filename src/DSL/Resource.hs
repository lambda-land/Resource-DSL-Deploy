{-# LANGUAGE UndecidableInstances #-}

module DSL.Resource where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Control.Monad.Catch (MonadCatch)
import Control.Monad.Reader
import Control.Monad.State

import DSL.Environment
import DSL.Primitive

import {-# SOURCE #-} DSL.Effect  (Effect)
import {-# SOURCE #-} DSL.Model   (Model)
import {-# SOURCE #-} DSL.Profile (Profile)


-- 
-- * Evaluation Monad
--

-- | Variable environment.
type VarEnv = Env Var PVal

-- | Resource environment.
type ResEnv = Env Path PVal -- TODO make variational

-- | Dictionary of profiles and models.
type Dictionary = Env Var (Either Profile Model)

-- | Reader context for evaluation.
data Context = Ctx {
    prefix      :: Path,       -- ^ resource path prefix
    environment :: VarEnv,     -- ^ variable environment
    dictionary  :: Dictionary  -- ^ dictionary of profiles and models
} deriving (Data,Eq,Generic,Read,Show,Typeable)

type EvalM a = StateT ResEnv (ReaderT Context IO) a

-- | A monad for computations that affect a resource environment, given a
--   an evaluation context, and which may throw/catch exceptions.
class (MonadCatch m, MonadReader Context m, MonadState ResEnv m)
  => MonadEval m
instance (MonadCatch m, MonadReader Context m, MonadState ResEnv m)
  => MonadEval m

-- | Execute a computation in a given context.
runInContext :: Context -> ResEnv -> EvalM a -> IO (a, ResEnv)
runInContext ctx init mx = runReaderT (runStateT mx init) ctx

-- | Execute a computation with a given dictionary and an empty variable
--   environment and prefix.
runWithDict :: Dictionary -> ResEnv -> EvalM a -> IO (a, ResEnv)
runWithDict dict = runInContext (Ctx [] envEmpty dict)

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
