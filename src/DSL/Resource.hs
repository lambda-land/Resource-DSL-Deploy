module DSL.Resource where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Composition ((.:))
import Data.Maybe (fromJust)
import Data.SBV

import DSL.Types
import DSL.Environment
import DSL.Name
import DSL.Path
import DSL.SAT


-- | Execute a computation in a given context.
runInContext :: Context -> StateCtx -> EvalM a -> Either VError (a, StateCtx)
runInContext ctx init mx = runExcept (runReaderT (runStateT mx init) ctx)

-- | Execute a computation with a given dictionary and an empty variable
--   environment and prefix.
runWithDict :: Dictionary -> StateCtx -> EvalM a -> Either VError (a, StateCtx)
runWithDict dict = runInContext (Ctx (ResID []) envEmpty dict (BLit True))

-- | Execute a computation in the empty context.
runInEmptyContext :: StateCtx -> EvalM a -> Either VError (a, StateCtx)
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

getMask :: MonadEval m => m Mask
getMask = gets mask

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
  throwError (One e)

allErrors :: Mask -> Bool
allErrors (One Nothing) = False
allErrors (One (Just _)) = True
allErrors (Chc _ l r) = allErrors l && allErrors r

throwMask :: MonadEval m => Mask -> m a
throwMask m = throwError $ fmap fromJust m

traverseMask :: BExpr -> Mask -> Mask
traverseMask _ m@(One _) = m
traverseMask d (Chc d' l r) = if d `implies` d' then
                                traverseMask d l
                              else
                                traverseMask d r

checkMask :: MonadEval m => BExpr -> m ()
checkMask d = do
  m <- getMask
  let m' = traverseMask d m
  if allErrors m' then return () else throwMask m'

vAlt :: MonadEval m => BExpr -> Value -> (PVal -> m Value) -> m Value
vAlt d v f = do
  c <- getVCtx
  let c' = c &&& d
  checkMask c'
  local (\(Ctx p m d _) -> Ctx p m d c') (vBind (return v) f)

vBind :: MonadEval m => m Value -> (PVal -> m Value) -> m Value
vBind v f = do
  v' <- v
  case v' of
    (One p) -> f p
    (Chc d l r) -> do
      let r' = vAlt (bnot d) r f
      l' <- vAlt d l f `catchError` (\e -> do
        r' `catchError` (\e' ->
          if e == e' then
            throwError e
          else
            throwError $ Chc d e e')
        return $ One PErr)
      r'' <- r' `catchError` (\_ -> (return $ One PErr))
      return $ Chc d l' r''
