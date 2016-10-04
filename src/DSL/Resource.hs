{-# LANGUAGE UndecidableInstances #-}

module DSL.Resource where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Control.Monad.Catch (MonadCatch)
import Control.Monad.Reader
import Control.Monad.State
import Data.Composition ((.:))

import DSL.Environment
import DSL.Name
import DSL.Path
import DSL.Primitive

import {-# SOURCE #-} DSL.Model   (Model)
import {-# SOURCE #-} DSL.Profile (Profile)


--
-- * Dictionary
--

-- | Dictionary entry.
data Entry
     = ProEntry Profile
     | ModEntry Model
  deriving (Data,Eq,Generic,Read,Show,Typeable)

-- | Dictionary of profiles and models.
type Dictionary = Env CompID Entry

-- Merge duplicate dictionary entries. For now, merges profiles w/ profiles
-- and models w/ models, otherwise throws an error.
instance MergeDup Entry where
  mergeDup (ProEntry l) (ProEntry r) = ProEntry (mergeDup l r)
  mergeDup (ModEntry l) (ModEntry r) = ModEntry (mergeDup l r)
  mergeDup _ _ = error "mergeDup (Entry): cannot merge profile and model"


-- 
-- * Evaluation Monad
--

-- | Variable environment.
type VarEnv = Env Var PVal

-- | Resource environment.
type ResEnv = Env ResID PVal  -- TODO make variational

-- | Reader context for evaluation.
data Context = Ctx {
    prefix      :: ResID,      -- ^ resource ID prefix
    environment :: VarEnv,     -- ^ variable environment
    dictionary  :: Dictionary  -- ^ dictionary of profiles and models
} deriving (Data,Eq,Generic,Read,Show,Typeable)

-- Throw an error if we attempt to merge two primitive values.
instance MergeDup PVal where
  mergeDup _ _ = error "mergeDup (PVal): attempted to merge duplicate entries"

-- | A class of monads for computations that affect a resource environment,
--   given an evaluation context, and which may throw/catch exceptions.
class (MonadCatch m, MonadReader Context m, MonadState ResEnv m)
  => MonadEval m
instance (MonadCatch m, MonadReader Context m, MonadState ResEnv m)
  => MonadEval m

-- | A specific monad for running MonadEval computations.
type EvalM a = StateT ResEnv (ReaderT Context IO) a

-- | Execute a computation in a given context.
runInContext :: Context -> ResEnv -> EvalM a -> IO (a, ResEnv)
runInContext ctx init mx = runReaderT (runStateT mx init) ctx

-- | Execute a computation with a given dictionary and an empty variable
--   environment and prefix.
runWithDict :: Dictionary -> ResEnv -> EvalM a -> IO (a, ResEnv)
runWithDict dict = runInContext (Ctx (ResID []) envEmpty dict)

-- | Execute a computation in the empty context.
runInEmptyContext :: ResEnv -> EvalM a -> IO (a, ResEnv)
runInEmptyContext = runWithDict envEmpty

-- | Get the current resource ID prefix.
getPrefix :: MonadEval m => m ResID
getPrefix = asks prefix

-- | Convert a path to a resource ID, using the prefix if the path is relative.
getResID :: MonadEval m => Path -> m ResID
getResID path = do
    pre <- getPrefix
    toResID pre path

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

-- | Execute a computation with a specific prefix.
withPrefix :: MonadEval m => ResID -> m a -> m a
withPrefix p = local (\(Ctx _ m d) -> Ctx p m d)

-- | Execute a computation with an updated value environment.
withVarEnv :: MonadEval m => (VarEnv -> VarEnv) -> m a -> m a
withVarEnv f = local (\(Ctx p m d) -> Ctx p (f m) d)

-- | Execute a computation with an extended value environment.
withNewVar :: MonadEval m => Var -> PVal -> m a -> m a
withNewVar = withVarEnv .: envExtend

-- | Update the resource environment.
updateResEnv :: MonadEval m => (ResEnv -> ResEnv) -> m ()
updateResEnv = modify
