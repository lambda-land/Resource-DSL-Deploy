{-# LANGUAGE UndecidableInstances #-}

module DSL.Evaluation where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Control.Applicative (Alternative(..))
import Control.Monad.Fail
import Control.Monad.Reader
import Control.Monad.State
import Data.Composition ((.:))
import Data.Maybe (fromMaybe)
import Data.SBV.Internals (SolverContext(..))
import Data.SBV.Trans
import Data.SBV.Trans.Control
import Data.Set (Set)
import Data.Text (unpack)

import DSL.Boolean
import DSL.Types
import DSL.Environment
import DSL.Path
import DSL.Predicate
import DSL.Primitive


--
-- * Evaluation context
--

-- | The dimension environment associates symbolic values with each name that
--   occurs in the condition of a choice.
type DimEnv = (Env Var SBool, Env Var SInt32)

-- | The resource environment tracks the values of globally scoped resources.
type ResEnv = Env ResID Value

-- | The variable environment tracks the values of locally scoped variables.
type VarEnv = Env Var Value

-- | Reader context for evaluation.
data Context = Ctx {
  dictionary  :: Dictionary, -- ^ dictionary of models
  dimensions  :: DimEnv,     -- ^ symbolic values for boolean dimensions
  environment :: VarEnv,     -- ^ variable environment
  prefix      :: ResID,      -- ^ resource ID prefix
  vCtx        :: BExpr       -- ^ current variational context
} deriving (Eq,Generic,Show,Typeable)

-- | State context for evaluation.
data StateCtx = SCtx {
  resEnv :: ResEnv,          -- ^ the resource environment
  abort  :: Bool,            -- ^ whether to abort this branch of the execution
  errCtx :: BExpr,           -- ^ variation context of errors that have occurred
  vError :: VError           -- ^ variational error
} deriving (Data,Eq,Generic,Ord,Read,Show,Typeable)

-- | Resulting context of a successful computation.
data SuccessCtx = SuccessCtx {
  successCtx  :: BExpr,      -- ^ the variants that succeeded
  configSpace :: Set Var     -- ^ dimensions in the configuration space
} deriving (Data,Eq,Generic,Ord,Read,Show,Typeable)

-- | Initialize a context with the given dictionary.
withDict :: Dictionary -> Context
withDict dict = Ctx dict (envEmpty,envEmpty) envEmpty (ResID []) (BLit True)

-- | Initialize a state context with the given resource environment.
withResEnv :: ResEnv -> StateCtx
withResEnv renv = SCtx renv False (BLit False) (One Nothing)


--
-- * Evaluation monad
--

-- | Requirements of an evaluation monad.
type MonadEval m = (MonadReader Context m, MonadState StateCtx m)

-- | A monad for running variational computations in an evaluation monad.
newtype EvalM a = EvalM {
  unEvalM :: StateT StateCtx (ReaderT Context Query) (VOpt a)
}

-- | Execute a computation in the given context.
runEval :: Context -> StateCtx -> EvalM a -> IO (VOpt a, StateCtx)
runEval ctx init (EvalM mx) = runSMTWith (z3 { verbose = True }) (query (runReaderT (runStateT mx init) ctx))

-- | Execute a computation on the given inputs with initialized contexts.
runEvalWith
  :: Dictionary  -- ^ dictionary of models
  -> ResEnv      -- ^ initial resource environment
  -> Set Var     -- ^ set of boolean dimensions
  -> Set Var     -- ^ set of integer dimensions
  -> EvalM a     -- ^ computation to run
  -> IO (VOpt a, StateCtx)
runEvalWith dict renv bs is mx =
    runEval (withDict dict) (withResEnv renv) (withDims bs is mx)

-- | Helper function for returning plain values from within EvalM.
plain :: Monad m => a -> m (VOpt a)
plain = return . One . Just

instance Functor EvalM where
  fmap f (EvalM mx) = EvalM (mx >>= return . fmap (fmap f))

instance Applicative EvalM where
  pure a = EvalM (pure (One (Just a)))
  (<*>) = ap

instance Monad EvalM where
  EvalM mx >>= f = EvalM (mx >>= unEvalM . mapVOpt f)

instance Alternative EvalM where
  empty = EvalM (pure (One Nothing))
  l <|> r = EvalM $ do
    l' <- unEvalM l
    case l' of
      One Nothing -> unEvalM r
      _ -> pure l'

instance MonadPlus EvalM

instance MonadFail EvalM where
  fail _ = EvalM (return (One Nothing))

instance MonadIO EvalM where
  liftIO mx = EvalM (liftIO mx >>= plain)

instance MonadReader Context EvalM where
  ask = EvalM (ask >>= plain)
  local f (EvalM mx) = EvalM (local f mx)

instance MonadState StateCtx EvalM where
  get = EvalM (get >>= plain)
  put s = EvalM (put s >> plain ())

instance MonadQuery EvalM where
  queryState = EvalM (queryState >>= plain)

instance MonadSymbolic EvalM where
  symbolicEnv = EvalM (symbolicEnv >>= plain)

instance SolverContext EvalM where
  constrain b = EvalM (constrain b >> plain ())
  softConstrain b = EvalM (softConstrain b >> plain ())
  namedConstraint s b = EvalM (namedConstraint s b >> plain ())
  constrainWithAttribute as b = EvalM (constrainWithAttribute as b >> plain ())
  setOption o = EvalM (setOption o >> plain ())
  contextState = EvalM (contextState >>= plain)

instance (Monad m, SolverContext m) => SolverContext (StateT s m) where
  constrain b = lift (constrain b)
  softConstrain b = lift (softConstrain b)
  namedConstraint s b = lift (namedConstraint s b)
  constrainWithAttribute as b = lift (constrainWithAttribute as b)
  setOption o = lift (setOption o)
  contextState = lift contextState

instance (Monad m, SolverContext m) => SolverContext (ReaderT s m) where
  constrain b = lift (constrain b)
  softConstrain b = lift (softConstrain b)
  namedConstraint s b = lift (namedConstraint s b)
  constrainWithAttribute as b = lift (constrainWithAttribute as b)
  setOption o = lift (setOption o)
  contextState = lift contextState


-- ** Core operations

-- | Execute a computation with new dimensions in the dimension environment.
withDims :: Set Var -> Set Var -> EvalM a -> EvalM a
withDims bs is mx = do
    -- traceM "%%%% ADDING DIMS %%%%"
    bm <- symEnv (sBool . unpack) bs
    im <- symEnv (sInt32 . unpack) is
    local (\c -> c { dimensions = (bm,im) }) mx

-- | Execute a computation in an extended variation context.
inVCtx :: BExpr -> EvalM a -> EvalM a
inVCtx (BLit True) mx = mx
inVCtx e mx = case shrinkBExpr e of
    BLit True -> mx
    e' -> do
      s <- toSymbolic e
      inVCtxSym e' s mx

-- | A version of 'inVCtx' that takes the already-generated symbolic value
--   for the boolean expression.
inVCtxSym :: BExpr -> SBool -> EvalM a -> EvalM a
inVCtxSym e s mx = inNewAssertionStack $ do
    constrain s
    local (\c -> c { vCtx = shrinkBExpr (e &&& vCtx c) }) mx

-- | Extract the resulting variational value from an evaluation action.
reflectV :: EvalM a -> EvalM (VOpt a)
reflectV (EvalM mx) = EvalM (mx >>= return . One . Just)

-- | Map an evaluation over a variational value.
mapV :: (a -> EvalM b) -> V a -> EvalM b
mapV f va = EvalM $ do
    b <- getAbort
    if b then return (One Nothing)
    else go true va
  where
    go c (One a)     = unEvalM (inVCtx c (f a))
    go c (Chc d l r) = do
      l' <- go (d &&& c) l
      lb <- getAbort
      setAbort False
      r' <- go (bnot d &&& c) r
      rb <- getAbort
      if lb && rb then return (One Nothing)
      else setAbort False >> return (Chc d l' r')

-- | Map an evaluation over a variational optional value.
mapVOpt :: (a -> EvalM b) -> VOpt a -> EvalM b
mapVOpt f va = EvalM $ do
    b <- getAbort
    if b then return (One Nothing)
    else go true va
  where
    go _ (One Nothing)  = return (One Nothing)
    go c (One (Just a)) = unEvalM (inVCtx c (f a))
    go c (Chc d l r)    = do
      l' <- go (d &&& c) l
      lb <- getAbort
      setAbort False
      r' <- go (bnot d &&& c) r
      rb <- getAbort
      if lb && rb then return (One Nothing)
      else setAbort False >> return (Chc d l' r')

-- | Execute a computation if we have not aborted this branch.
checkAbort :: EvalM a -> EvalM a
checkAbort mx = EvalM $ do
    b <- getAbort
    if b then return (One Nothing) else unEvalM mx

-- | Record an error in the current variation context and return an empty
--   value.
returnError :: Error -> EvalM a
returnError e = EvalM $ do
    c <- getVCtx
    setAbort True
    updateErrCtx (c |||)
    updateVError (Chc c (One (Just e)))
    return (One Nothing)

-- | Consume a value that may have failed, recording an error if so.
handleError :: Either Error a -> EvalM a
handleError (Right a) = return a
handleError (Left e)  = returnError e


--
-- * Getters
--

-- | Get the dictionary of models.
getDict :: MonadEval m => m Dictionary
getDict = asks dictionary

-- | Get the dimension environment.
getDimEnv :: MonadEval m => m DimEnv
getDimEnv = asks dimensions

-- | Get the current variable environment.
getVarEnv :: MonadEval m => m VarEnv
getVarEnv = asks environment

-- | Get the current resource ID prefix.
getPrefix :: MonadEval m => m ResID
getPrefix = asks prefix

-- | Get the current variational context.
getVCtx :: MonadEval m => m BExpr
getVCtx = asks vCtx

-- | Get the current resource environment.
getResEnv :: MonadEval m => m ResEnv
getResEnv = gets resEnv

-- | Get the current abort flag value.
getAbort :: MonadEval m => m Bool
getAbort = gets abort

-- | Get the current error context.
getErrCtx :: MonadEval m => m BExpr
getErrCtx = gets errCtx

-- | Get the current variational error.
getVError :: MonadEval m => m VError
getVError = gets vError


-- ** Setters

-- | Execute a computation with an updated value environment.
withVarEnv :: MonadEval m => (VarEnv -> VarEnv) -> m a -> m a
withVarEnv f = local (\c -> c { environment = f (environment c) })

-- | Execute a computation with an extended value environment.
withNewVar :: MonadEval m => Var -> Value -> m a -> m a
withNewVar = withVarEnv .: envExtend

-- | Execute a computation with a value environment extended by
--   a list of var-value pairs.
withNewVars :: MonadEval m => [(Var,Value)] -> m a -> m a
withNewVars = withVarEnv . envExtends

-- | Execute a computation with a specific prefix.
withPrefix :: MonadEval m => ResID -> m a -> m a
withPrefix p = local (\c -> c { prefix = p })

-- | Update the resource environment.
updateResEnv :: MonadEval m => (ResEnv -> ResEnv) -> m ()
updateResEnv f = modify (\s -> s { resEnv = f (resEnv s) })

-- | Set the abort flag.
setAbort :: MonadEval m => Bool -> m ()
setAbort a = modify (\s -> s { abort = a })

-- | Update the error context.
updateErrCtx :: MonadEval m => (BExpr -> BExpr) -> m ()
updateErrCtx f = modify (\s -> s { errCtx = f (errCtx s) })

-- | Update the variational error.
updateVError :: MonadEval m => (VError -> VError) -> m ()
updateVError f = modify (\s -> s { vError = f (vError s)})


-- ** Queries

-- | Lookup a component in the dictionary.
compLookup :: Var -> EvalM Model
compLookup x = do
    d <- getDict
    case envLookup x d of
      Nothing -> returnError (CompNotFound x)
      Just m -> return m  -- TODO: do we need to mask this?

-- | Lookup a variable in the variable environment.
varLookup :: Var -> EvalM PVal
varLookup x = do
    m <- getVarEnv
    case envLookup x m of
      Nothing -> returnError (VarNotFound x)
      Just v -> shrinkValue v >>= EvalM . return

-- | Convert a path to a resource ID, using the prefix if the path is relative.
getResID :: Path -> EvalM ResID
getResID path = do
    pre <- getPrefix
    handleError (toResID pre path)

-- | Lookup a resource in the resource environment.
resLookup :: ResID -> EvalM PVal
resLookup rID = do
    m <- getResEnv
    case envLookup rID m of
      Nothing -> returnError (ResNotFound rID)
      Just v -> shrinkValue v >>= EvalM . return


--
-- * SAT interface
--

-- | Get the symbolic value for a boolean expression.
toSymbolic :: BExpr -> EvalM SBool
toSymbolic e = do
    (bs,is) <- getDimEnv
    return (toSBool bs is e)

-- | Is the predicate satisfiable in the current context?
isSat :: SBool -> EvalM Bool
isSat s = checkSatAssuming [s] >>= \case
    Sat -> return True
    Unsat -> return False
    Unk -> error "Internal error: solver returned \"unknown\""

-- | Is the predicate unsatisfiable in the current context?
isUnsat :: SBool -> EvalM Bool
isUnsat = fmap not . isSat

-- | Is the predicate a tautology in the current context?
isTaut :: SBool -> EvalM Bool
isTaut = isUnsat . bnot


--
-- * Variational values
--

-- | Partially configure a value using the given boolean expression.
selectValue :: BExpr -> Value -> EvalM Value
selectValue d v = inVCtx d (shrinkValue v)

-- | Partially configure a value using the given boolean expression and its
--   encoding as a symbolic value.
selectValueSym :: BExpr -> SBool -> Value -> EvalM Value
selectValueSym d s v = inVCtxSym d s (shrinkValue v)

-- | Shrink a value in the current variation context by eliminating dead
--   alternatives and applying some basic restructuring rules.
shrinkValue :: Value -> EvalM Value
shrinkValue (One a) = return (One a)
shrinkValue (Chc (BLit True)  l _) = shrinkValue l
shrinkValue (Chc (BLit False) _ r) = shrinkValue r
shrinkValue (Chc (OpB Not d) l r)  = shrinkValue (Chc d r l)
shrinkValue (Chc d l r) = do
    let d' = shrinkBExpr d
    sd <- toSymbolic d'
    taut <- isTaut sd
    if taut then shrinkValue l
    else do
      unsat <- isUnsat sd
      if unsat then shrinkValue r
      else do
        l' <- selectValueSym d' sd l
        r' <- selectValueSym (bnot d') (bnot sd) r
        return (Chc d' l' r')


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
execBlock ss = checkAbort (mapM_ (checkAbort . execStmt) ss)

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
    v <- shrinkValue (Chc ctx new old)
    updateResEnv (envExtend rID v)

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
