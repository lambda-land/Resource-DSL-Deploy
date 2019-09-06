module DSL.Expression where

import Prelude hiding (LT,GT)

import Control.Monad (zipWithM)

import DSL.Types
import DSL.Resource
import DSL.Value
import DSL.V
import DSL.Primitive
import DSL.Environment
import DSL.Pretty ()


--
-- * Expressions
--

(??) :: V Expr -> (V Expr, V Expr) -> Expr
c ?? (t,e) = P3 Cond c t e

infix 1 ??

-- function to test for ArgTypeError Constructor
isArgTypeError :: ExprError -> Bool
isArgTypeError (ArgTypeError _ _ _ _) = True
isArgTypeError _ = False


-- ** Semantics

-- | Evaluate an expression.
evalExpr :: MonadEval m => Expr -> VM m PVal
evalExpr (Ref x) = VM $ do
    env <- getVarEnv
    envLookupV
      (ExprE . VarNotFound . NF)
      (\k x y -> ExprE . VarNotFound $ VNF k x y)
      x env
evalExpr (Res p) = VM $ do
    rID <- getResID p
    env <- getResEnv
    envLookupV
      (ExprE . ResNotFound . NF)
      (\k x y -> ExprE . ResNotFound $ VNF k x y)
      rID env
evalExpr (Lit v)      = VM (return (toVMaybe v))
evalExpr (P1 o e)     = applyPrim1 o (evalExprV e)
evalExpr (P2 o l r)   = applyPrim2 o (evalExprV l) (evalExprV r)
evalExpr (P3 o c t e) = applyPrim3 o (evalExprV c) (evalExprV t) (evalExprV e)

evalExprV :: MonadEval m => V Expr -> VM m PVal
evalExprV = evalExprVM . VM . return . toVMaybe

evalExprVM :: MonadEval m => VM m Expr -> VM m PVal
evalExprVM m = m >>= evalExpr

-- | Check the type of an argument. Implicitly converts integer arguments
--   to floats, if needed.
primCheckArg :: Param -> Value -> PType -> PVal -> Either Error PVal
primCheckArg _ _ TFloat (I i) = Right (F . fromIntegral $ i)
primCheckArg p v t pv | primType pv == t = Right pv
                      | otherwise = Left . ExprE $ ArgTypeError p v t pv

checkArgVM :: MonadEval m => Param -> Value -> VM m PType -> VM m PVal -> VM m PVal
checkArgVM p v t u = do
  t' <- t
  u' <- u
  toVM $ promoteError (primCheckArg p v t' u')

checkArg :: MonadEval m => Param -> Value -> m Value
checkArg p@(Param _ t) v = unVM $ checkArgVM p v (VM . return . toVMaybe $ t) (VM . return $ v)

-- | Evaluate a function.
evalFun :: MonadEval m => Fun -> Value -> m Value
evalFun (Fun p@(Param x _) e) v = do
    v' <- checkArg p v
    withNewVar x v' (unVM . evalExprV $ e)

-- | Run a computation in a variable environment extended by new arguments.
withArgs :: MonadEval m => [Param] -> [V Expr] -> m a -> m a
withArgs xs args go = do
    vals <- mapM (unVM . evalExprV) args
    new  <- fmap envFromList (zipWithM envBuilder xs vals)
    withVarEnv (envUnion new) go
  where
    envBuilder p@(Param var _) v = checkArg p v >>= \v' -> return (var, v')
