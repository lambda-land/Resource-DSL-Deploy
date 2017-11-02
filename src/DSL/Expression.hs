module DSL.Expression where

import Prelude hiding (LT,GT)

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Control.Monad (zipWithM)
import Control.Monad.Catch (Exception,throwM)

import DSL.Types
import DSL.Environment
import DSL.Name
import DSL.Path
import DSL.Primitive
import DSL.Resource


--
-- * Expressions
--

-- ** Syntax


(??) :: Expr -> (Expr,Expr) -> Expr
c ?? (t,e) = P3 Cond c t e

infix 1 ??

-- function to test for ArgTypeError Constructor
isArgTypeError :: ArgTypeError -> Bool
isArgTypeError (ArgTypeError _ _) = True

-- ** Semantics

-- | Evaluate an expression.
evalExpr :: MonadEval m => Expr -> m PVal
evalExpr (Ref x)      = getVarEnv >>= envLookup x
evalExpr (Res p)      = do rID <- getResID p
                           env <- getResEnv
                           envLookup rID env
evalExpr (Lit v)      = return v
evalExpr (P1 o e)     = evalExpr e >>= primOp1 o
evalExpr (P2 o l r)   = do l' <- evalExpr l
                           r' <- evalExpr r
                           primOp2 o l' r'
evalExpr (P3 o c t e) = do c' <- evalExpr c
                           t' <- evalExpr t
                           e' <- evalExpr e
                           primOp3 o c' t' e'
-- evalExpr (Chc p l r)  = liftM2 (ChcV p) (evalExpr l) (evalExpr r)

-- | Check the type of an argument. Implicitly converts integer arguments
--   to floats, if needed.
checkArg :: MonadEval m => Param -> PVal -> m (Var,PVal)
checkArg (Param x TFloat) (I i) = return (x, F (fromIntegral i))
checkArg p@(Param x t) v
    | primType v == t = return (x,v)
    | otherwise       = throwM (ArgTypeError p v)

-- | Evaluate a function.
evalFun :: MonadEval m => Fun -> PVal -> m PVal
evalFun (Fun p@(Param x _) e) v = do
    checkArg p v
    withNewVar x v (evalExpr e)

-- | Run a computation in a variable environment extended by new arguments.
withArgs :: MonadEval m => [Param] -> [Expr] -> m a -> m a
withArgs xs args go = do
    vals <- mapM evalExpr args
    new  <- fmap envFromList (zipWithM checkArg xs vals)
    withVarEnv (envUnion new) go
