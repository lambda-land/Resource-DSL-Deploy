module DSL.Expression where

import Prelude hiding (LT,GT)

import Control.Monad (zipWithM)

import DSL.Types
import DSL.Environment
import DSL.Name
import DSL.Resource
import DSL.Value


--
-- * Expressions
--


(??) :: Expr -> (Expr,Expr) -> Expr
c ?? (t,e) = P3 Cond c t e

infix 1 ??

-- function to test for ArgTypeError Constructor
isArgTypeError :: ArgTypeError -> Bool
isArgTypeError (ArgTypeError _ _) = True

-- ** Semantics

-- | Evaluate an expression.
evalExpr :: MonadEval m => Expr -> m Value
evalExpr (Ref x)      = getVarEnv >>= (\env -> promoteError (envLookup x env))
evalExpr (Res p)      = do rID <- getResID p
                           env <- getResEnv
                           promoteError (envLookup rID env)
evalExpr (Lit v)      = return v
evalExpr (P1 o e)     = applyPrim1 o (evalExpr e)

evalExpr (P2 o l r)   = applyPrim2 o (evalExpr l) (evalExpr r)
evalExpr (P3 o c t e) = applyPrim3 o (evalExpr c) (evalExpr t) (evalExpr e)

vEvalExpr :: MonadEval m => m (V Expr) -> m Value
vEvalExpr e = vBind e evalExpr

-- | Check the type of an argument. Implicitly converts integer arguments
--   to floats, if needed.
checkArg :: MonadEval m => Param -> Value -> m (Var,Value)
checkArg = undefined -- TODO
{-
checkArg (Param x TFloat) (I i) = return (x, F (fromIntegral i))
checkArg p@(Param x t) v
    | primType v == t = return (x,v)
    | otherwise       = vError (ExprE $ ArgTypeError p v)
-}
-- | Evaluate a function.
evalFun :: MonadEval m => Fun -> Value -> m Value
evalFun (Fun p@(Param x _) e) v = do
    checkArg p v
    withNewVar x v (evalExpr e)

-- | Run a computation in a variable environment extended by new arguments.
withArgs :: MonadEval m => [Param] -> [Expr] -> m a -> m a
withArgs xs args go = do
    vals <- mapM evalExpr args
    new  <- fmap envFromList (zipWithM checkArg xs vals)
    withVarEnv (envUnion new) go
