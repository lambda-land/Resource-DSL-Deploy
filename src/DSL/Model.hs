{-# LANGUAGE UndecidableInstances #-}

module DSL.Model where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Control.Monad        (foldM)
import Control.Monad.Catch  (MonadCatch)
import Control.Monad.Reader (MonadReader,asks,local,runReaderT)

import DSL.Environment
import DSL.Expression
import DSL.Predicate
import DSL.Resource
import DSL.Value


--
-- * Components
--

-- ** Syntax

-- | An application model.
data Model = Model [Var] Block
  deriving (Data,Eq,Generic,Read,Show,Typeable)

-- | Statement block.
type Block = [Stmt]

-- | Statement in an application model.
data Stmt
     = Do Name Effect       -- ^ apply an effect
     | In Path Block        -- ^ do work in a sub-environment
     | If Pred Block Block  -- ^ conditional statement
     | Sub Name [Expr]      -- ^ load a sub-model or profile
  deriving (Data,Eq,Generic,Read,Show,Typeable)


-- ** Semantics

-- | A monad that supports executing statements and loading models.
class (MonadCatch m, MonadReader StmtContext m) => MonadExec m
instance (MonadCatch m, MonadReader StmtContext m) => MonadExec m

-- | Dictionary of profiles and models.
type Dictionary = Env (Either Profile Model)

-- | Context for executing a statement.
data StmtContext = Ctx {
    prefix      :: Path,       -- ^ resource path prefix
    environment :: Env Value,  -- ^ value environment
    dictionary  :: Dictionary  -- ^ dictionary of profiles and models
} deriving (Data,Eq,Generic,Read,Show,Typeable)

-- | Get the current resource path prefix.
getPrefix :: MonadExec m => m Path
getPrefix = asks prefix

-- | Get the current value environment.
getEnv :: MonadExec m => m (Env Value)
getEnv = asks environment

-- | Get the current dictionary of profiles and models.
getDict :: MonadExec m => m Dictionary
getDict = asks dictionary

-- | Execute a computation with an updated prefix.
withPrefix :: MonadExec m => (Path -> Path) -> m a -> m a
withPrefix f = local (\(Ctx p m d) -> Ctx (f p) m d)

-- | Execute a computation with an updated value environment.
withEnv :: MonadExec m => (Env Value -> Env Value) -> m a -> m a
withEnv f = local (\(Ctx p m d) -> Ctx p (f m) d)

-- | Execute a computation with an updated value environment.
withDict :: MonadExec m => (Dictionary -> Dictionary) -> m a -> m a
withDict f = local (\(Ctx p m d) -> Ctx p m (f d))

-- | Run a monadic action in the simpler MonadEval.
-- runEval :: (MonadEval m1, MonadExec m2) => m1 a -> m2 a
runEval x = asks environment >>= runReaderT x

-- | Evaluate an expression within the extended statement context.
evalExprInStmt :: MonadExec m => Expr -> m Value
evalExprInStmt e = getEnv >>= runReaderT (evalExpr e)

-- | Load a model into the current environment, prefixed by the given path.
loadModel :: MonadExec m => Model -> [Expr] -> REnv -> m REnv
loadModel (Model xs block) args env = do
    vals <- mapM evalExprInStmt args
    let new = envFromList (zip xs vals)
    withEnv (envUnion new) (execBlock block env)

-- | Execute a block of statements.
execBlock :: MonadExec m => Block -> REnv -> m REnv
execBlock stmts renv = foldM (flip execStmt) renv stmts

-- | Execute a statement.
execStmt :: MonadExec m => Stmt -> REnv -> m REnv
-- apply an effect
execStmt (Do n eff) renv = do
    pre <- getPrefix
    getEnv >>= runReaderT (resolveEffect (pre ++ [n]) eff renv)
-- do work in sub-environment
execStmt (In path block) renv = withPrefix (++ path) (execBlock block renv)
-- conditional statement
execStmt (If cond tru fls) renv = error "execStmt: If not implemented"
-- load a sub-module or profile
execStmt (Sub name args) renv = do
    def <- asks dictionary >>= envLookup name
    case def of
      Left profile -> undefined -- loadProfile profile args renv
      Right model  -> loadModel model args renv
