module DSL.SAT where

import Data.Data (Typeable)
import GHC.Generics (Generic)

import Control.Exception (Exception,throwIO)
import Control.Monad.IO.Class (liftIO)
import Z3.Monad

import DSL.Types (Cond)


--
-- * SAT interface
--

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
