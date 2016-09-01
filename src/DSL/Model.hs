module DSL.Model where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import DSL.Effect
import DSL.Environment
import DSL.Expression
import DSL.Predicate
import DSL.Profile
import DSL.Resource


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

-- | Load a model into the current environment, prefixed by the given path.
loadModel :: MonadEval m => Model -> [Expr] -> m ()
loadModel (Model xs block) args = do
    vals <- mapM evalExpr args
    let new = envFromList (zip xs vals)
    withVarEnv (envUnion new) (execBlock block)

-- | Execute a block of statements.
execBlock :: MonadEval m => Block -> m ()
execBlock stmts = mapM_ execStmt stmts

-- | Execute a statement.
execStmt :: MonadEval m => Stmt -> m ()
-- apply an effect
execStmt (Do n eff) = do
    pre <- getPrefix
    resolveEffect (pre ++ [n]) eff
-- do work in sub-environment
execStmt (In path block) = withPrefix (++ path) (execBlock block)
-- conditional statement
execStmt (If cond tru fls) = error "execStmt: If not yet implemented"
-- load a sub-module or profile
execStmt (Sub name args) = do
    def <- getDict >>= envLookup name
    case def of
      Left profile -> loadProfile profile args
      Right model  -> loadModel model args
