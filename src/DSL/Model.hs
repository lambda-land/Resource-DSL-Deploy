module DSL.Model where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import DSL.Effect
import DSL.Environment
import DSL.Expression
import DSL.Predicate
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

-- | A condition in a conditional statement.
type Cond = BExpr

-- | Statement in an application model.
data Stmt
     = Do Name Effect       -- ^ apply an effect
     | In Path Block        -- ^ do work in a sub-environment
     | If Cond Block Block  -- ^ conditional statement
     | Load Name [Expr]     -- ^ load a sub-model or profile
  deriving (Data,Eq,Generic,Read,Show,Typeable)

-- | Check whether a resource is present.
checkPresent :: Name -> Stmt
checkPresent n = Do n (Check ("x",true))

-- | Provide a unit-valued resource.
provideUnit :: Name -> Stmt
provideUnit n = Do n (Create (Lit Unit))


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
loadModel (Model xs block) args = withArgs (map fst xs) args (execBlock block)

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
execStmt (If cond tru fls) = error "execStmt: If not yet implemented"
-- load a sub-module or profile
execStmt (Load name args) = do
    def <- getDict >>= envLookup name
    case def of
      Left profile -> loadProfile profile args
      Right model  -> loadModel model args
