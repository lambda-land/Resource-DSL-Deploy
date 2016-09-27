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
     | Load Name [Expr]     -- ^ load a sub-model or profile
  deriving (Data,Eq,Generic,Read,Show,Typeable)

-- | Type error caused by non-boolean condition.
data StmtError = IfTypeError Expr
  deriving (Data,Eq,Generic,Read,Show,Typeable)

instance Exception StmtError

-- | Check whether a unit-valued resource is present.
checkUnit :: Path -> Stmt
checkUnit p = Do p (Check (Fun (Param "x" TUnit) true))

-- | Provide a unit-valued resource.
provideUnit :: Path -> Stmt
provideUnit p = Do p (Create (Lit Unit))

-- | Macro for an integer-case construct. Evaluates the expression, then
--   compares the resulting integer value against each case in turn, executing
--   the first matching block, otherwise executes the final block arugment.
caseOf :: Expr -> [(Int,Block)] -> Block -> Stmt
caseOf expr cases other = Let x expr (foldr ifs other cases)
  where
    ifs (i,thn) els = [If (Ref x .== Lit (I i)) thn els]
    x = "$case"


-- ** Operations

-- | Convert a simple model into a profile. This allows writing profiles
--   with nicer syntax. Fails with a runtime error on a Load or If statement.
toProfile :: Model -> Profile
toProfile (Model xs stmts) = Profile xs (envFromListAcc (concatMap (entries root) stmts))
  where
    entries pre (In path blk) | Just rID <- toResID pre path = concatMap (entries rID) blk
    entries pre (Do path eff) | Just rID <- toResID pre path = [(rID, [eff])]
    entries _ _ = error "toProfile: cannot convert model to profile"

-- | Construct a profile dictionary from an association list of models.
profileDict :: [(Name,Model)] -> Dictionary
profileDict l = envFromList [(n, ProEntry (toProfile m)) | (n,m) <- l]

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

-- | Execute a block of statements.
execBlock :: MonadEval m => Block -> m ()
execBlock stmts = mapM_ execStmt stmts

-- | Convert a path to a resource ID.
getResID :: MonadEval m => Path -> m ResID
getResID path = do
    pre <- getPrefix
    toResID pre path

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
execStmt (Load name args) = do
    def <- getDict >>= envLookup name
    case def of
      ProEntry profile -> loadProfile profile args
      ModEntry model   -> loadModel model args
