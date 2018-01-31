module DSL.Model where

import Control.Monad (forM_)

import DSL.Types
import DSL.Effect
import DSL.Environment
import DSL.Expression
import DSL.Name
import DSL.Path
import DSL.Profile
import DSL.Resource
import DSL.SegList


--
-- * Components
--



-- ** Operations

-- | Construct a model dictionary from an association list of models.
modelDict :: [(Name,Model)] -> Dictionary
modelDict l = envFromList [(Symbol n, ModEntry m) | (n,m) <- l]

-- | Construct a profile dictionary from an association list of models.
profileDict :: [(Name,Model)] -> Dictionary
profileDict l = envFromList [(Symbol n, ProEntry (toProfile m)) | (n,m) <- l]

{- TODO TODO TODO
-- | Convert a simple model into a profile. This allows writing profiles
--   with nicer syntax. Fails with a runtime error on a Load or If statement.
toProfile :: Model -> Profile
toProfile (Model xs vstmts) =
    Profile xs (envFromListAcc (concatMap (entries pathThis) vstmts))
  where
    entries pre (In path blk) = concatMap (entries (pathAppend pre path)) blk
    entries pre (One . Just $ (Do path eff)) = [(pathAppend pre path, [eff])]
    entries _ _ = error "toProfile: cannot convert model to profile"
-}
toProfile :: Model -> Profile
toProfile = undefined



-- TODO: convert profiles to models, compose profiles and models


-- ** Semantics

-- | Load a model into the current environment, prefixed by the given path.
loadModel :: MonadEval m => Model -> [V Expr] -> m ()
loadModel (Model xs block) args = withArgs xs args (execBlock block)

-- | Load a component by ID.
loadComp :: MonadEval m => CompID -> [V Expr] -> m ()
loadComp cid args = do
    dict <- getDict
    def <- promoteError (envLookup cid dict)
    case def of
      ProEntry profile -> loadProfile profile args
      ModEntry model   -> loadModel model args

-- | Execute a block of statements.
execBlock :: MonadEval m => Block -> m ()
execBlock = segMapM_ execStmt

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
      _ -> VM $ vError (StmtE $ StmtError stmt IfTypeError val)) >> return ()
-- do work in sub-environment
execStmt (In path body) = execInSub path (execBlock body)
-- loop over indexed sub-environments
execStmt stmt@(For var expr body) = unVM (do
    let iter i = execInSub (pathFor i) (withNewVar var (One. Just . I $ i) (execBlock body))
    val <- evalExprV expr
    case val of
      I n -> toVM $ forM_ [1..n] iter
      _ -> VM $ vError (StmtE $ StmtError stmt ForTypeError val)) >> return ()
-- extend the variable environment
execStmt (Let var expr body) = do
    val <- unVM $ evalExprV expr
    withNewVar var val (execBlock body)
-- load a sub-module or profile
execStmt stmt@(Load comp args) = unVM (do
    res <- evalExprV comp
    case res of
      S cid -> toVM $ loadComp cid args
      _ -> VM $ vError (StmtE $ StmtError stmt LoadTypeError res)) >> return ()
