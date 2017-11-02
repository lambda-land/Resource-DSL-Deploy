module DSL.Resource where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Composition ((.:))

import DSL.Types
import DSL.Environment
import DSL.Name
import DSL.Path


-- | Execute a computation in a given context.
runInContext :: Context -> StateCtx -> EvalM a -> Either Error (a, StateCtx)
runInContext ctx init mx = runExcept (runReaderT (runStateT mx init) ctx)

-- | Execute a computation with a given dictionary and an empty variable
--   environment and prefix.
runWithDict :: Dictionary -> StateCtx -> EvalM a -> Either Error (a, StateCtx)
runWithDict dict = runInContext (Ctx (ResID []) envEmpty dict (BLit True))

-- | Execute a computation in the empty context.
runInEmptyContext :: StateCtx -> EvalM a -> Either Error (a, StateCtx)
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
getResEnv = gets renv

getVCtx :: MonadEval m => m BExpr
getVCtx = asks vCtx

-- | Query the dictionary.
queryDict :: MonadEval m => (Dictionary -> a) -> m a
queryDict f = fmap f getDict

-- | Query the variable environment.
queryVarEnv :: MonadEval m => (VarEnv -> a) -> m a
queryVarEnv f = fmap f getVarEnv

-- | Query the current resource environment.
queryResEnv :: MonadEval m => (ResEnv -> a) -> m a
queryResEnv f = fmap f getResEnv

-- | Execute a computation with a specific prefix.
withPrefix :: MonadEval m => ResID -> m a -> m a
withPrefix p = local (\(Ctx _ m d v) -> Ctx p m d v)

-- | Execute a computation with an updated value environment.
withVarEnv :: MonadEval m => (VarEnv -> VarEnv) -> m a -> m a
withVarEnv f = local (\(Ctx p m d v) -> Ctx p (f m) d v)

-- | Execute a computation with an extended value environment.
withNewVar :: MonadEval m => Var -> Value -> m a -> m a
withNewVar = withVarEnv .: envExtend

-- | Update the resource environment.
updateResEnv :: MonadEval m => (ResEnv -> ResEnv) -> m ()
updateResEnv f = modify (\(SCtx r m) -> SCtx (f r) m)

updateMask :: MonadEval m => (Mask -> Mask) -> m ()
updateMask f = modify (\(SCtx r m) -> SCtx r (f m))

combineMasks :: BExpr -> Error -> Mask -> Mask
combineMasks b e m = Chc b (One (Just e)) m

vError :: MonadEval m => Error -> m a
vError e = do
  c <- getVCtx
  updateMask (combineMasks c e)
  throwError e
