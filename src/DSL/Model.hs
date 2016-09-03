module DSL.Model where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Control.Monad.Catch (Exception,throwM)

import DSL.Effect
import DSL.Environment
import DSL.Expression
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
     = Do Name Effect       -- ^ apply an effect
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
checkUnit :: Name -> Stmt
checkUnit n = Do n (Check (Fun ("x",TUnit) true))

-- | Provide a unit-valued resource.
provideUnit :: Name -> Stmt
provideUnit n = Do n (Create (Lit Unit))

-- | Macro for an integer-case construct. Evaluates the expression, then
--   compares the resulting integer value against each case in turn, executing
--   the first matching block, otherwise executes the final block arugment.
caseOf :: Expr -> [(Int,Block)] -> Block -> Stmt
caseOf expr cases other = Let x expr (foldr ifs other cases)
  where
    ifs (i,thn) els = [If (Ref x .== Lit (I i)) thn els]
    x = "$case"

-- ** Conversion

-- | Convert a simple model into a profile. This allows writing profiles
--   with nicer syntax. Fails with a runtime error on a Load or If statement.
toProfile :: Model -> Profile
toProfile (Model xs stmts) = Profile xs (envFromListAcc (stmts >>= entries []))
  where
    entries pre (Do name eff) = [(pre ++ [name], [eff])]
    entries pre (In path blk) = blk >>= entries (pre ++ path)
    entries _ _ = error "toProfile: cannot convert models with If/Load to profiles"

-- | Construct a profile dictionary from an association list of models.
profileDict :: [(Name,Model)] -> Dictionary
profileDict l = envFromList [(n, Left (toProfile m)) | (n,m) <- l]


-- ** Semantics

-- | Load a model into the current environment, prefixed by the given path.
loadModel :: MonadEval m => Model -> [Expr] -> m ()
loadModel (Model xs block) args = withArgs xs args (execBlock block)

-- | Execute a block of statements.
execBlock :: MonadEval m => Block -> m ()
execBlock stmts = mapM_ execStmt stmts

-- | Execute a statement.
execStmt :: MonadEval m => Stmt -> m ()
-- apply an effect
execStmt (Do name eff) = do
    pre <- getPrefix
    resolveEffect (pre ++ [name]) eff
-- do work in sub-environment
execStmt (In path block) = withPrefix (++ path) (execBlock block)
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
      Left profile -> loadProfile profile args
      Right model  -> loadModel model args
