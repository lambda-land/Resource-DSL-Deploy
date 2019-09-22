module DSL.SAT where

import Data.Data (Typeable)
import GHC.Generics (Generic)

import Control.Exception (Exception,throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (unpack)
import Z3.Monad
import qualified Z3.Base as Z3B

import DSL.Types (Cond,Env,Var)
import DSL.Environment (envFromList,envLookup)


--
-- * SAT interface
--

-- ** Initialization

-- | Initialize and return a solver instance and a new context.
initSolver :: IO (Solver, Context)
initSolver = do
    cfg <- Z3B.mkConfig
    setOpts cfg stdOpts
    ctx <- Z3B.mkContext cfg
    z3  <- Z3B.mkSolver ctx
    return (z3,ctx)


-- ** Symbol environments

-- | Types of symbolic values.
data SymType = SymBool | SymInt
  deriving (Eq,Generic,Ord,Read,Show,Typeable)

-- | An environment mapping typed variables to symbolic values.
type SymEnv = Env (Var,SymType) AST

-- | Given sets of boolean and integer variables, construct an environment
--   with fresh symbolic values for each variable.
symEnv :: MonadZ3 m => Set Var -> Set Var -> m SymEnv
symEnv bs is = fmap envFromList (mapM sym xs)
  where
    xs = Set.toList (Set.map (,SymBool) bs <> Set.map (,SymInt) is)
    sym k@(x,SymBool) = symBool x >>= \s -> return (k,s)
    sym k@(x,SymInt)  = symInt  x >>= \s -> return (k,s)

-- | Create a fresh symbolic boolean variable.
symBool :: MonadZ3 m => Var -> m AST
symBool x = mkStringSymbol (unpack x) >>= mkBoolVar

-- | Create a fresh symbolic integer variable.
symInt :: MonadZ3 m => Var -> m AST
symInt x = mkStringSymbol (unpack x) >>= mkIntVar


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
    push
    assert s
    (_,r) <- getModel
    pop 1
    return r

-- | Find several different variable assignments that satisfy the given
--   boolean expression. TODO: Currently only returning the first one!
satModels :: MonadZ3 m => Int -> AST -> m [Model]
satModels _ s = satModel s >>= return . maybe [] (:[])

-- | Read the value of a boolean variable in a SAT model.
boolVal :: MonadZ3 m => Var -> SymEnv -> Model -> m (Maybe Bool)
boolVal x env mod = case envLookup (x,SymBool) env of
    Just s -> evalBool mod s
    _ -> return Nothing

-- | Read the value of an integer variable in a SAT model.
intVal :: MonadZ3 m => Var -> SymEnv -> Model -> m (Maybe Int)
intVal x env mod = case envLookup (x,SymInt) env of
    Just s -> evalInt mod s >>= return . fmap fromIntegral
    _ -> return Nothing


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
