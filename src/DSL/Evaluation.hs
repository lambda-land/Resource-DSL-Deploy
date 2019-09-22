{-# LANGUAGE UndecidableInstances #-}

module DSL.Evaluation where

import Data.Data (Typeable)
import GHC.Generics (Generic)

import Control.Applicative (Alternative(..))
import Control.Monad.Fail
import Control.Monad.Reader
import Control.Monad.State
import Data.Composition ((.:))
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Z3.Base as Z3B
import qualified Z3.Monad as Z3

import DSL.Boolean
import DSL.Condition
import DSL.Environment
import DSL.Path
import DSL.Primitive
import DSL.SAT
import DSL.Types


--
-- * Evaluation context
--

-- | The resource environment tracks the values of globally scoped resources.
type ResEnv = Env ResID Value

-- | The variable environment tracks the values of locally scoped variables.
type VarEnv = Env Var Value

-- | Reader context for evaluation.
data ReaderCtx = RCtx {
  z3Solver    :: Z3.Solver,  -- ^ solver reference
  z3Context   :: Z3.Context, -- ^ solver context
  dictionary  :: Dictionary, -- ^ dictionary of models
  environment :: VarEnv,     -- ^ variable environment
  prefix      :: ResID,      -- ^ resource ID prefix
  vCtx        :: Cond        -- ^ current variational context
} deriving (Eq,Generic,Show,Typeable)

-- | State context for evaluation.
data StateCtx = SCtx {
  resEnv :: ResEnv,          -- ^ the resource environment
  abort  :: Bool,            -- ^ whether to abort this branch of the execution
  errCtx :: Cond,            -- ^ variation context of errors that have occurred
  vError :: VError           -- ^ variational error
} deriving (Eq,Generic,Ord,Read,Show,Typeable)

-- | Resulting context of a successful computation.
data SuccessCtx = SuccessCtx {
  successCtx  :: BExpr,      -- ^ the variants that succeeded
  configSpace :: Set Var     -- ^ dimensions in the configuration space
} deriving (Eq,Generic,Ord,Read,Show,Typeable)


--
-- * Evaluation monad
--

-- | Requirements of an evaluation monad.
type MonadEval m = (MonadReader ReaderCtx m, MonadState StateCtx m, Z3.MonadZ3 m)

-- | A monad for running variational computations in an evaluation monad.
newtype EvalM a = EvalM {
  unEvalM :: StateT StateCtx (ReaderT ReaderCtx IO) (VOpt a)
}

-- | Execute a computation on the given inputs with initialized contexts.
runEval
  :: Z3.Solver   -- ^ solver reference
  -> Z3.Context  -- ^ solver context
  -> Dictionary  -- ^ dictionary of models
  -> ResEnv      -- ^ initial resource environment
  -> EvalM a     -- ^ computation to run
  -> IO (VOpt a, StateCtx)
runEval z3 ctx dict renv (EvalM mx) = do
    tru <- Z3B.mkTrue ctx
    fls <- Z3B.mkFalse ctx
    let r = RCtx z3 ctx dict envEmpty (ResID []) (Cond true (Just tru))
    let s = SCtx renv False (Cond false (Just fls)) (One Nothing)
    runReaderT (runStateT mx s) r

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

instance MonadReader ReaderCtx EvalM where
  ask = EvalM (ask >>= plain)
  local f (EvalM mx) = EvalM (local f mx)

instance MonadState StateCtx EvalM where
  get = EvalM (get >>= plain)
  put s = EvalM (put s >> plain ())

instance (Applicative m, Monad m, MonadIO m, MonadReader ReaderCtx m) => Z3.MonadZ3 m where
  getSolver = asks z3Solver
  getContext = asks z3Context

instance Show Z3.Context where
  show _ = "<z3-context>"

instance Show Z3.Solver where
  show _ = "<z3-solver>"


-- ** Core operations

-- | Execute a computation in an extended variation context.
inVCtx :: Cond -> EvalM a -> EvalM a
inVCtx (Cond (BLit True) _) mx = mx
inVCtx new@(Cond _ (Just s)) mx = do
    old <- getVCtx
    Z3.local $ do
      Z3.assert s
      c <- condAnd new old
      local (\r -> r { vCtx = c }) mx
inVCtx c _ = errorUnprepped c

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
returnError err = EvalM $ do
    c <- getVCtx
    setAbort True
    updateErrCtx (condOr c)
    updateVError (Chc c (One (Just err)))
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

-- | Get the current variable environment.
getVarEnv :: MonadEval m => m VarEnv
getVarEnv = asks environment

-- | Get the current resource ID prefix.
getPrefix :: MonadEval m => m ResID
getPrefix = asks prefix

-- | Get the current variational context.
getVCtx :: MonadEval m => m Cond
getVCtx = asks vCtx

-- | Get the current resource environment.
getResEnv :: MonadEval m => m ResEnv
getResEnv = gets resEnv

-- | Get the current abort flag value.
getAbort :: MonadEval m => m Bool
getAbort = gets abort

-- | Get the current error context.
getErrCtx :: MonadEval m => m Cond
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

-- | Set the error contextj.
setErrCtx :: MonadEval m => Cond -> m ()
setErrCtx c = modify (\s -> s { errCtx = c })

-- | Update the error context.
updateErrCtx :: MonadEval m => (Cond -> m Cond) -> m ()
updateErrCtx f = do
    c <- getErrCtx
    c' <- f c
    setErrCtx c'

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
-- * Variational values
--

-- | Partially configure a value using the given boolean expression.
selectValue :: Cond -> Value -> EvalM Value
selectValue d v = inVCtx d (shrinkValue v)

-- | Shrink a value in the current variation context by eliminating dead
--   alternatives and applying some basic restructuring rules.
shrinkValue :: Value -> EvalM Value
shrinkValue (One a) = return (One a)
shrinkValue (Chc c@(Cond e (Just s)) l r) =
    case e of
      BLit True  -> shrinkValue l
      BLit False -> shrinkValue r
      OpB Not e' -> do
        s' <- Z3.mkNot s
        shrinkValue (Chc (Cond e' (Just s')) l r)
      _ -> do
        taut <- isTaut s
        if taut then shrinkValue l
        else do
          unsat <- isUnsat s
          if unsat then shrinkValue r
          else do
            c' <- condNot c
            l' <- selectValue c l
            r' <- selectValue c' r
            return (Chc (Cond (shrinkBExpr e) (Just s)) l' r')
shrinkValue (Chc c _ _) = errorUnprepped c


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
