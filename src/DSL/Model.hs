module DSL.Model where

import DSL.Types
import DSL.Effect
import DSL.Environment
import DSL.Expression
import DSL.Resource


--
-- * Components
--

-- ** Semantics

-- | Load a model into the current environment, prefixed by the given path.
loadModel :: MonadEval m => Model -> [V Expr] -> m ()
loadModel (Model xs block) args = withArgs xs args (execBlock block)

-- | Load a component by ID.
loadComp :: MonadEval m => Name -> [V Expr] -> m ()
loadComp cid args = do
    dict <- getDict
    model <- promoteError (envLookup cid dict)
    loadModel model args

-- | Execute a block of statements.
execBlock :: MonadEval m => Block -> m ()
execBlock = mapM_ execStmt

-- | Execute a command in a sub-environment.
execInSub :: MonadEval m => Path -> m a -> m a
execInSub path mx = do
    rID <- getResID path
    withPrefix rID mx

-- | Execute a statement.
execStmt :: MonadEval m => Stmt -> m ()
-- apply an effect
execStmt (Do path eff) = do
    rID <- getResID path
    resolveEffect rID eff
-- conditional statement
execStmt stmt@(If cond tru fls) = unVM (do
    val <- evalExprV cond
    case val of
      B True  -> toVM $ execBlock tru
      B False -> toVM $ execBlock fls
      _ -> VM $ vThrowError (StmtE $ StmtError stmt IfTypeError val)) >> return ()
-- do work in sub-environment
execStmt (In path body) = execInSub path (execBlock body)
-- extend the variable environment
execStmt (Let var expr body) = do
    val <- unVM $ evalExprV expr
    withNewVar var val (execBlock body)
-- load a sub-module or profile
execStmt stmt@(Load comp args) = unVM (do
    res <- evalExprV comp
    case res of
      S cid -> toVM $ loadComp cid args
      _ -> VM $ vThrowError (StmtE $ StmtError stmt LoadTypeError res)) >> return ()
