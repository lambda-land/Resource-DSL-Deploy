module DSL.SAT where

import Data.Data (Typeable)
import GHC.Generics (Generic)

import Control.Exception (Exception,throwIO)
import Control.Monad.Reader hiding (local)
import Data.Foldable (foldrM)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (unpack)
import Z3.Monad
import qualified Z3.Base as Z3B

import DSL.Types (Cond,Env,OptType(..),Var)
import DSL.Environment


--
-- * SAT interface
--

-- ** Initialization

-- | SAT solver context.
type SatCtx = (Solver,Context)

-- | Initialize and return a solver instance and a new context.
initSolver :: IO SatCtx
initSolver = do
    cfg <- Z3B.mkConfig
    -- Some helpful options for z3 debugging.
    -- Z3B.setParamValue cfg "debug_ref_count" "true"
    -- Z3B.setParamValue cfg "trace" "true"
    -- Z3B.setParamValue cfg "well_sorted_check" "true"
    setOpts cfg stdOpts
    ctx <- Z3B.mkContext cfg
    z3  <- Z3B.mkSolverForLogic ctx Z3B.QF_LIA
    return (z3,ctx)


-- ** Symbol environments

-- | An environment mapping typed configuration options to symbolic values.
type SymEnv = Env (Var,OptType) AST

-- | Given sets of boolean and integer variables, construct an environment
--   with fresh symbolic values for each variable.
symEnvFresh :: SatCtx -> Set Var -> Set Var -> IO SymEnv
symEnvFresh (_,ctx) bs is = fmap envFromList (mapM sym xs)
  where
    xs = Set.toList (Set.map (,OptBool) bs <> Set.map (,OptInt) is)
    sym k@(x,t) = do
      s <- Z3B.mkStringSymbol ctx (unpack x)
      case t of
        OptBool -> Z3B.mkBoolVar ctx s >>= return . (k,)
        OptInt  -> Z3B.mkIntVar  ctx s >>= return . (k,)


-- ** SAT monad

-- | A monad for interacting with the SAT solver.
type SatM a = ReaderT SatCtx IO a

-- | Execute a computation in the SAT monad.
runSat :: SatCtx -> SatM a -> IO a
runSat = flip runReaderT

instance MonadZ3 (ReaderT SatCtx IO) where
  getSolver = asks fst
  getContext = asks snd


-- ** Sat checking

-- | Is the symbolic condition satisfiable in the current context?
isSat :: MonadZ3 m => AST -> m Bool
isSat s = checkAssumptions [s] >>= \case
    Sat   -> return True
    Unsat -> return False
    Undef -> errorUndefined

-- | Is the symbolic condition unsatisfiable in the current context?
isUnsat :: MonadZ3 m => AST -> m Bool
isUnsat = fmap not . isSat

-- | Is the symbolic condition a tautology in the current context?
isTaut :: MonadZ3 m => AST -> m Bool
isTaut s = mkNot s >>= isUnsat


-- ** Sat solving

-- | Find a variable assignment that satisfies the given boolean expression.
satModel :: MonadZ3 m => AST -> m (Maybe Model)
satModel s = do
    (_,r) <- local (assert s >> getModel)
    return r

-- | Find several different variable assignments that satisfy the given
--   boolean expression. TODO: Currently only returning the first one!
satModels :: MonadZ3 m => Int -> AST -> m [Model]
satModels _ s = satModel s >>= return . maybe [] (:[])

-- | Read the value of a boolean variable in a SAT model.
boolVal :: MonadZ3 m => Var -> SymEnv -> Model -> m (Maybe Bool)
boolVal x env mod = case envLookup (x,OptBool) env of
    Just s -> evalBool mod s
    _ -> return Nothing

-- | Read the value of an integer variable in a SAT model.
intVal :: MonadZ3 m => Var -> SymEnv -> Model -> m (Maybe Int)
intVal x env mod = case envLookup (x,OptInt) env of
    Just s -> evalInt mod s >>= return . fmap fromIntegral
    _ -> return Nothing

-- | Return value environments for the given symbols in a SAT model.
satResult :: MonadZ3 m => SymEnv -> Model -> m (Env Var Bool, Env Var Int)
satResult env mod = foldrM go (envEmpty,envEmpty) (envToList env)
  where
    go ((x,OptBool),s) (mb,mi) = evalBool mod s
      >>= maybe (err x) (\b -> return (envExtend x b mb, mi))
    go ((x,OptInt),s) (mb,mi) = evalInt mod s
      >>= maybe (err x) (\i -> return (mb, envExtend x (fromIntegral i) mi))
    err x = error $ "Undefined variable when building result: " ++ unpack x


-- ** Errors

-- | An error encountered during SAT solving. These are thrown as IO
--   exceptions rather than managed through the usual variational
--   error system since they reflect either an implementation bug or
--   some kind of system error, not a problem in user code.
data SolverError = SolverError String
  deriving (Eq,Generic,Ord,Read,Show,Typeable)

instance Exception SolverError

-- | Throw a solver error.
throwSolverError :: MonadZ3 m => String -> m a
throwSolverError = liftIO . throwIO . SolverError

-- | The solver returned undefined.
errorUndefined :: MonadZ3 m => m a
errorUndefined = throwSolverError "solver returned \"undefined\""

-- | Encountered a condition that was not prepped before evaluating.
errorUnprepped :: MonadZ3 m => Cond -> m a
errorUnprepped c = throwSolverError ("condition not prepped for solver: " ++ show c)


-- ** Instances

-- These instances aren't that useful, but make it possible to derive 'Show'
-- for other types that contain them.

instance Show Context where
  show _ = "<z3-context>"

instance Show Solver where
  show _ = "<z3-solver>"
