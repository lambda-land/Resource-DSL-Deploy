{-# LANGUAGE UndecidableInstances #-}

module DSL.Resource where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Composition ((.:))
import Data.SBV (bnot, (&&&), (|||))
import Data.Typeable

import DSL.Types
import DSL.SAT
import DSL.Name
import DSL.Path
import DSL.Environment
import DSL.V


-- | Execute a computation in a given context.
runInContext :: Context -> StateCtx -> EvalM a -> (Either Mask a, StateCtx)
runInContext ctx init mx = runReader (runStateT (runExceptT mx) init) ctx
--runInContext ctx init mx = runExcept $ runReaderT (runStateT mx init) ctx

-- | Execute a computation with a given dictionary and an empty variable
--   environment and prefix.
runWithDict :: Dictionary -> StateCtx -> EvalM a -> (Either Mask a, StateCtx)
runWithDict dict = runInContext (Ctx (ResID []) envEmpty dict (BLit True))

-- | Execute a computation in the empty context.
runInEmptyContext :: StateCtx -> EvalM a -> (Either Mask a, StateCtx)
runInEmptyContext = runWithDict envEmpty

runEmpty :: EvalM a -> (Either Mask a, StateCtx)
runEmpty = runInEmptyContext (SCtx envEmpty (BLit False) (One Nothing))

-- | Take a value in the plain monadic evaluation context and
--   turn it into a varitional value in the same context.
toVM :: MonadEval m => m a -> VM m a
toVM m = VM $ m >>= return . One . Just

-- | Get the current resource ID prefix.
getPrefix :: MonadEval m => m ResID
getPrefix = asks prefix

-- | Convert a path to a resource ID, using the prefix if the path is relative.
getResID :: MonadEval m => Path -> m ResID
getResID path = do
    pre <- getPrefix
    promoteError (toResID pre path)

-- | Get the current dictionary of profiles and models.
getDict :: MonadEval m => m Dictionary
getDict = asks dictionary

-- | Get the current variable environment.
getVarEnv :: MonadEval m => m VarEnv
getVarEnv = asks environment

-- | Get the current resource environment.
getResEnv :: MonadEval m => m ResEnv
getResEnv = gets renv

-- | Get the current mask.
getMask :: MonadEval m => m Mask
getMask = gets mask

-- | Get the current error context.
getErrCtx :: MonadEval m => m BExpr
getErrCtx = gets errCtx

-- | Get the current variational context.
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
updateResEnv f = modify (\(SCtx r e m) -> SCtx (f r) e m)

-- | Update the error context.
updateErrCtx :: MonadEval m => (BExpr -> BExpr) -> m ()
updateErrCtx f = modify (\(SCtx r e m) -> SCtx r (f e) m)

-- | Update the mask.
updateMask :: MonadEval m => (Mask -> Mask) -> m ()
updateMask f = modify (\(SCtx r e m) -> SCtx r e (f m))

vMove :: MonadEval m => BExpr -> m a -> m a
vMove d m = do
  c <- getVCtx
  e <- getErrCtx
  let c' = c &&& d
  if (c' |=>| e) then
    throwError (One Nothing)
  else
    local (\(Ctx p m dict _) -> Ctx p m dict c') m

vHandleUnit' :: MonadEval m => BExpr -> m () -> m () -> m ()
vHandleUnit' d l r = do
  let r' = vMove (bnot d) r
  let l' = vMove d l
  l' `catchError` (\e -> do
    r' `catchError` (\e' -> throwError (Chc d e e'))
    return ())
  r' `catchError` (\_ -> return ())
  return ()

vHandleUnit :: MonadEval m => BExpr -> (a -> m ()) -> a -> a -> m ()
vHandleUnit d f l r = vHandleUnit' d (f l) (f r)

vHandle' :: MonadEval m => BExpr -> m (V (Maybe a)) -> m (V (Maybe a)) -> m (V (Maybe a))
vHandle' d l r = do
  let r' = vMove (bnot d) r
  let l' = vMove d l
  l'' <- l' `catchError` (\e -> do
    r' `catchError` (\e' -> throwError (Chc d e e'))
    return (One Nothing))
  r'' <- r' `catchError` (\_ -> return (One Nothing))
  return (Chc d l'' r'')

vHandle :: MonadEval m => BExpr -> (V (Maybe a) -> m (V (Maybe b))) -> V (Maybe a) -> V (Maybe a) -> m (V (Maybe b))
vHandle d f l r = vHandle' d (f l) (f r)

lookupHelper :: (MonadEval m) => (BExpr -> Error) -> V (Maybe a) -> m (V (Maybe a))
lookupHelper g (One Nothing) = do
  c <- getVCtx
  vError (g c)
lookupHelper _ v@(One (Just _)) = return v
lookupHelper g (Chc d l r) = vHandle d (lookupHelper g) l r

envLookupV :: (Ord k, MonadEval m, Show k, Typeable k) =>
              (NotFound -> Error) ->
              (BExpr -> (V (Maybe v)) -> Error) ->
              k -> Env k (V (Maybe v)) -> m (V (Maybe v))
envLookupV f g k env = do
  case envLookup k env of
    -- throw appropriate error for whole context
    Left (EnvE nf) -> vError (f nf)
    Right v -> do
      -- throw errors for malformed contexts
      let g' = (\d -> g d v)
      c <- getVCtx
      let v' = select c v
      lookupHelper g' v'
    _ -> error "The impossible happened."



-- | Look up a resource in a variational context, returning a
--   variational value. Note that it does not throw errors for
--   nonexistent resources, so error handling should be handled
--   separately.
resLookup :: MonadEval m => ResID -> m Value
resLookup rID = do
  re <- getResEnv
  case envLookup' rID re of
    Nothing -> return (One Nothing)
    Just v -> do
      c <- getVCtx
      return $ select c v -- select on the result for the current variational context

combineMasks :: BExpr -> Error -> Mask -> Mask
combineMasks b e m = mergeMask m (Chc b (One (Just e)) (One Nothing))

vError :: MonadEval m => Error -> m a
vError e = do
  c <- getVCtx
  updateErrCtx (\bexpr -> bexpr ||| c)
  updateMask (combineMasks c e)
  throwError (One (Just e))

promoteError :: MonadEval m => Either Error a -> m a
promoteError (Right a) = return a
promoteError (Left e) = vError e

vBind :: MonadEval m => m (V (Maybe a)) -> (a -> m (V (Maybe b))) -> m (V (Maybe b))
vBind v f = do
  v' <- v
  -- c <- getVCtx
  -- let v'' = select c v' TODO: should we have this?
  case v' of
    (One Nothing) -> return (One Nothing)
    (One (Just a)) -> f a
    (Chc d l r) -> vHandle d (\v -> vBind (return v) f) l r

instance (MonadEval m) => Functor (VM m) where
  fmap = liftM

instance (MonadEval m) => Applicative (VM m) where
  pure = return
  (<*>) = ap

instance (MonadEval m) => Monad (VM m) where
  return = VM . return . One . Just
  v >>= f = VM $ vBind (unVM v) (unVM . f)
