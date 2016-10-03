module DSL.Model where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Control.Monad.Catch (Exception,throwM)
import Data.List (union)
import Data.Monoid ((<>))

import DSL.Effect
import DSL.Environment
import DSL.Expression
import DSL.Name
import DSL.Path
import DSL.Primitive
import DSL.Profile
import DSL.Resource


--
-- * Components
--

-- ** Syntax

-- | An application model.
data Model = Model [Param] Block
  deriving (Data,Eq,Generic,Read,Show,Typeable)

-- | Statement block.
type Block = [Stmt]

-- | Statement in an application model.
data Stmt
     = Do Path Effect       -- ^ apply an effect
     | In Path Block        -- ^ do work in a sub-environment
     | If Expr Block Block  -- ^ conditional statement
     | Let Var Expr Block   -- ^ extend the variable environment
     | Load Expr [Expr]     -- ^ load a sub-model or profile
  deriving (Data,Eq,Generic,Read,Show,Typeable)

-- | Type errors in statements.
data StmtError
     = IfTypeError Expr     -- ^ non-boolean condition
     | LoadTypeError Expr   -- ^ not a component ID
  deriving (Data,Eq,Generic,Read,Show,Typeable)

instance Exception StmtError


-- ** Operations

-- | Convert a simple model into a profile. This allows writing profiles
--   with nicer syntax. Fails with a runtime error on a Load or If statement.
toProfile :: Model -> Profile
toProfile (Model xs stmts) =
    Profile xs (envFromListAcc (concatMap (entries pathThis) stmts))
  where
    entries pre (In path blk)
      | Just path' <- pathAppend pre path = concatMap (entries path') blk
    entries pre (Do path eff)
      | Just path' <- pathAppend pre path = [(path', [eff])]
    entries _ _ = error "toProfile: cannot convert model to profile"

-- | Construct a profile dictionary from an association list of models.
profileDict :: [(Name,Model)] -> Dictionary
profileDict l = envFromList [(Symbol n, ProEntry (toProfile m)) | (n,m) <- l]

-- | Compose two models by sequencing the statements in their bodies.
--   Merges parameters by name.
composeModels :: Model -> Model -> Model
composeModels (Model ps1 b1) (Model ps2 b2) =
    Model (union ps1 ps2) (b1 ++ b2)

instance MergeDup Model where
  mergeDup = composeModels

-- TODO: convert profiles to models, compose profiles and models


-- ** Semantics

-- | Load a model into the current environment, prefixed by the given path.
loadModel :: MonadEval m => Model -> [Expr] -> m ()
loadModel (Model xs block) args = withArgs xs args (execBlock block)

-- | Load a component by ID.
loadComp :: MonadEval m => CompID -> [Expr] -> m ()
loadComp cid args = do
    def <- getDict >>= envLookup cid
    case def of
      ProEntry profile -> loadProfile profile args
      ModEntry model   -> loadModel model args

-- | Execute a block of statements.
execBlock :: MonadEval m => Block -> m ()
execBlock stmts = mapM_ execStmt stmts

-- | Execute a statement.
execStmt :: MonadEval m => Stmt -> m ()
-- apply an effect
execStmt (Do path eff) = do
    rID <- getResID path
    resolveEffect rID eff
-- do work in sub-environment
execStmt (In path block) = do
    rID <- getResID path
    withPrefix rID (execBlock block)
-- conditional statement
execStmt (If cond tru fls) = do
    val <- evalExpr cond
    case val of
      B True  -> execBlock tru
      B False -> execBlock fls
      _ -> throwM (IfTypeError cond)
-- extend the variable environment
execStmt (Let var expr block) = do
    val <- evalExpr expr
    withVarEnv (envExtend var val) (execBlock block)
-- load a sub-module or profile
execStmt (Load comp args) = do
    res <- evalExpr comp
    case res of
      S cid -> loadComp cid args
      _ -> throwM (LoadTypeError comp)
