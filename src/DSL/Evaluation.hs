{-# LANGUAGE UndecidableInstances #-}

module DSL.Evaluation where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Control.Monad.Fail
import Control.Monad.Reader
import Control.Monad.State
import Data.Composition ((.:))
import Data.Maybe (fromMaybe)
import Data.Set (Set)

import DSL.Boolean
import DSL.Types
import DSL.Environment
import DSL.Path
import DSL.Primitive
import DSL.Variational


--
-- * Evaluation context
--

-- | Variable environment.
type VarEnv = Env Var Value

-- | Resource environment.
type ResEnv = Env ResID Value

-- | Reader context for evaluation.
data Context = Ctx {
  prefix      :: ResID,      -- ^ resource ID prefix
  environment :: VarEnv,     -- ^ variable environment
  dictionary  :: Dictionary, -- ^ dictionary of models
  vCtx        :: BExpr       -- ^ current variational context
} deriving (Data,Eq,Generic,Ord,Read,Show,Typeable)

-- | State context for evaluation.
data StateCtx = SCtx {
  resEnv :: ResEnv,          -- ^ the resource environment
  errCtx :: BExpr,           -- ^ variation context of errors that have occurred
  vError :: VError           -- ^ variational error
} deriving (Data,Eq,Generic,Ord,Read,Show,Typeable)

instance Variational StateCtx where
  configure c (SCtx r e m) = SCtx (configure c r) e (configure c m)
  select    c (SCtx r e m) = SCtx (select c r) e (select c m)
  shrink      (SCtx r e m) = SCtx (shrink r) e (shrink m)
  dimensions  (SCtx r _ m) = dimensions r <> dimensions m

-- | Resulting context of a successful computation.
data SuccessCtx = SuccessCtx {
  successCtx  :: BExpr,      -- ^ the variants that succeeded
  configSpace :: Set Var     -- ^ dimensions in the configuration space
} deriving (Data,Eq,Generic,Ord,Read,Show,Typeable)

-- | Initialize a context with the given dictionary.
withDict :: Dictionary -> Context
withDict dict = Ctx (ResID []) envEmpty dict (BLit True)

-- | Initialize a state context with the given resource environment.
withResEnv :: ResEnv -> StateCtx
withResEnv renv = SCtx renv (BLit False) (One Nothing)

-- | An initial context with no dictionary.
withNoDict :: Context 
withNoDict = withDict envEmpty


--
-- * Evaluation monad
--

-- | Requirements of an evaluation monad.
type MonadEval m = (MonadReader Context m, MonadState StateCtx m)

-- | A monad for running variational computations in an evaluation monad.
newtype EvalM a = EvalM { unEvalM :: StateT StateCtx (Reader Context) (VOpt a) }

-- | Execute a computation in the given context.
runEvalM :: Context -> StateCtx -> EvalM a -> (VOpt a, StateCtx)
runEvalM ctx init (EvalM mx) = runReader (runStateT mx init) ctx

instance Functor EvalM where
  fmap f (EvalM mx) = EvalM (mx >>= return . fmap (fmap f))

instance Applicative EvalM where
  pure a = EvalM (pure (One (Just a)))
  (<*>) = ap

instance Monad EvalM where
  EvalM mx >>= f = EvalM (mx >>= unEvalM . mapVOpt f)

instance MonadFail EvalM where
  fail _ = EvalM (return (One Nothing))

instance MonadReader Context EvalM where
  ask = EvalM (ask >>= return . One . Just)
  local f (EvalM mx) = EvalM (local f mx)

instance MonadState StateCtx EvalM where
  get = EvalM (get >>= return . One . Just)
  put s = EvalM (put s >> return (One (Just ())))


-- ** Core operations

-- | Extract the resulting variational value from an evaluation action.
reflectV :: EvalM a -> EvalM (VOpt a)
reflectV (EvalM mx) = EvalM (mx >>= return . One . Just)

-- | Map an evaluation over a variational value.
mapV :: (a -> EvalM b) -> V a -> EvalM b
mapV f va = EvalM $ do
    c <- getVCtx
    go c va
  where
    go c (One a)     = inVCtx c (unEvalM (f a))
    go c (Chc d l r) = do
      l' <- go (d &&& c) l
      r' <- go (bnot d &&& c) r
      return (Chc d l' r')

-- | Map an evaluation over a variational optional value.
mapVOpt :: (a -> EvalM b) -> VOpt a -> EvalM b
mapVOpt f va = EvalM $ do
    c <- getVCtx
    go c va
  where
    go _ (One Nothing)  = return (One Nothing)
    go c (One (Just a)) = inVCtx c (unEvalM (f a))
    go c (Chc d l r)    = do
      l' <- go (d &&& c) l
      r' <- go (bnot d &&& c) r
      return (Chc d l' r')

-- | Record an error in the current variation context and return an empty
--   value.
returnError :: Error -> EvalM a
returnError e = EvalM $ do
    c <- getVCtx
    updateErrCtx (c |||)
    updateVError (Chc c (One (Just e)))
    return (One Nothing)

-- | Consume a value that may have failed, recording an error if so.
handleError :: Either Error a -> EvalM a
handleError (Right a) = return a
handleError (Left e)  = returnError e


-- ** Getters, setters, etc.

-- | Get the current resource ID prefix.
getPrefix :: MonadEval m => m ResID
getPrefix = asks prefix

-- | Get the current dictionary of profiles and models.
getDict :: MonadEval m => m Dictionary
getDict = asks dictionary

-- | Get the current variable environment.
getVarEnv :: MonadEval m => m VarEnv
getVarEnv = asks environment

-- | Get the current resource environment.
getResEnv :: MonadEval m => m ResEnv
getResEnv = gets resEnv

-- | Get the current mask.
getVError :: MonadEval m => m VError
getVError = gets vError

-- | Get the current error context.
getErrCtx :: MonadEval m => m BExpr
getErrCtx = gets errCtx

-- | Get the current variational context.
getVCtx :: MonadEval m => m BExpr
getVCtx = asks vCtx

-- | Execute a computation in an extended variation context.
inVCtx :: MonadEval m => BExpr -> m a -> m a
inVCtx c' = local (\(Ctx p m d c) -> Ctx p m d (c' &&& c))

-- | Execute a computation with a specific prefix.
withPrefix :: MonadEval m => ResID -> m a -> m a
withPrefix p = local (\(Ctx _ m d v) -> Ctx p m d v)

-- | Execute a computation with an updated value environment.
withVarEnv :: MonadEval m => (VarEnv -> VarEnv) -> m a -> m a
withVarEnv f = local (\(Ctx p m d v) -> Ctx p (f m) d v)

-- | Execute a computation with an extended value environment.
withNewVar :: MonadEval m => Var -> Value -> m a -> m a
withNewVar = withVarEnv .: envExtend

-- | Execute a computation with a value environment extended by
--   a list of var-value pairs.
withNewVars :: MonadEval m => [(Var,Value)] -> m a -> m a
withNewVars = withVarEnv . envExtends

-- | Update the resource environment.
updateResEnv :: MonadEval m => (ResEnv -> ResEnv) -> m ()
updateResEnv f = modify (\(SCtx r e m) -> SCtx (f r) e m)

-- | Update the error context.
updateErrCtx :: MonadEval m => (BExpr -> BExpr) -> m ()
updateErrCtx f = modify (\(SCtx r e m) -> SCtx r (f e) m)

-- | Update the variational error.
updateVError :: MonadEval m => (VError -> VError) -> m ()
updateVError f = modify (\(SCtx r e m) -> SCtx r e (f m))


-- ** Queries

-- | Convert a path to a resource ID, using the prefix if the path is relative.
getResID :: Path -> EvalM ResID
getResID path = do
    pre <- getPrefix
    handleError (toResID pre path)

-- | Lookup a variable in the variable environment.
varLookup :: Var -> EvalM PVal
varLookup x = do
    c <- getVCtx
    m <- getVarEnv
    case envLookup x m of
      Nothing -> returnError (VarNotFound x)
      Just v -> EvalM (return (select c v))

-- | Lookup a component in the dictionary.
compLookup :: Var -> EvalM Model
compLookup x = do
    c <- getVCtx
    d <- getDict
    case envLookup x d of
      Nothing -> returnError (CompNotFound x)
      Just m -> return (select c m)

-- | Lookup a resource in the resource environment.
resLookup :: ResID -> EvalM PVal
resLookup rID = do
    c <- getVCtx
    m <- getResEnv
    case envLookup rID m of
      Nothing -> returnError (ResNotFound rID)
      Just v -> EvalM (return (select c v))


--
-- * Language semantics
--

-- ** Expressions

-- | Evaluate a function applied to the given argument.
evalFun :: Fun -> Value -> EvalM PVal
evalFun (Fun p e) v = withNewVar (paramName p) v (evalVExpr e)

-- | Evaluate an expression.
evalExpr :: Expr -> EvalM PVal
evalExpr (Ref x) = varLookup x
evalExpr (Res p) = getResID p >>= resLookup
evalExpr (Lit v) = EvalM (return (fmap Just v))
evalExpr (P1 o e1) = do
    v1 <- evalVExpr e1
    handleError (primOp1 o v1)
evalExpr (P2 o e1 e2) = do
    v1 <- evalVExpr e1
    v2 <- evalVExpr e2
    handleError (primOp2 o v1 v2)
evalExpr (P3 o e1 e2 e3) = do
    v1 <- evalVExpr e1
    v2 <- evalVExpr e2
    v3 <- evalVExpr e3
    handleError (primOp3 o v1 v2 v3)

-- | Evaluate a variational expression.
evalVExpr :: V Expr -> EvalM PVal
evalVExpr = mapV evalExpr


-- ** Models

-- | Load a model into the current environment, prefixed by the given path.
loadModel :: Model -> [V Expr] -> EvalM ()
loadModel (Model ps block) args = do
    vs <- mapM (reflectV . evalVExpr) args
    withNewVars (zip (map paramName ps) vs) (execBlock block)

-- | Load a component by ID.
loadComp :: Name -> [V Expr] -> EvalM ()
loadComp cid args = do
    model <- compLookup cid
    loadModel model args

-- | Execute a block of statements.
execBlock :: Block -> EvalM ()
execBlock = mapM_ execStmt

-- | Execute a statement.
execStmt :: Stmt -> EvalM ()
  -- apply an effect
execStmt (Do path eff) = do
    rID <- getResID path
    execEffect rID eff
  -- conditional statement
execStmt stmt@(If cond tru fls) = do
    val <- evalVExpr cond
    case val of
      B True  -> execBlock tru
      B False -> execBlock fls
      _ -> returnError (StmtError IfTypeError stmt val)
  -- do work in sub-environment
execStmt (In path body) = do
    rID <- getResID path
    withPrefix rID (execBlock body)
  -- extend the variable environment
execStmt (Let var expr body) = do
    val <- reflectV (evalVExpr expr)
    withNewVar var val (execBlock body)
  -- load a sub-module or profile
execStmt stmt@(Load comp args) = do
    res <- evalVExpr comp
    case res of
      S cid -> loadComp cid args
      _ -> returnError (StmtError LoadTypeError stmt res)

-- | Update a resource value in the current context.
updateRes :: ResID -> Value -> EvalM ()
updateRes rID new = do
    ctx <- getVCtx
    env <- getResEnv
    let old = fromMaybe (One Nothing) (envLookup rID env)
    updateResEnv (envExtend rID (shrink (Chc ctx new old)))

-- | Execute the effect on the given resource environment.
execEffect :: ResID -> Effect -> EvalM ()
  -- create
execEffect rID (Create e) = do
    val <- reflectV (evalVExpr e)
    updateRes rID val
  -- check
execEffect rID eff@(Check f) = do
    val <- reflectV (resLookup rID)
    res <- evalFun f val
    case res of
      B True  -> return ()
      B False -> returnError (EffectError CheckFailure eff rID val)
      _ -> returnError (EffectError CheckTypeError eff rID val)
  -- modify
execEffect rID (Modify f) = do
    val <- reflectV (resLookup rID)
    res <- reflectV (evalFun f val)
    updateRes rID res
  -- delete
execEffect rID Delete = updateRes rID (One Nothing) 
  -- TODO: actually delete if there are no variants left?
