{-# LANGUAGE FlexibleContexts #-}

module DSL.Semantics where

import Control.Monad (liftM2)
import Control.Monad.Except (MonadError,throwError)
import Control.Monad.Reader
import Control.Monad.State

import DSL.Environment
import DSL.Predicate
import DSL.Primitive
import DSL.Syntax


--
-- * Expression evaluation
--

-- | Evaluation monad.
type EvalM = ReaderT (Env Value) (Either String)

-- | Evaluate an expression.
evalExpr :: Expr -> EvalM Value
evalExpr (Lit v)     = return (Prim v)
evalExpr (P1 o e)    = evalExpr e >>= applyPrim1 o
evalExpr (P2 o l r)  = do l' <- evalExpr l
                          r' <- evalExpr r
                          applyPrim2 o l' r'
evalExpr (Ref x)     = ask >>= envLookup x
evalExpr (Fun x e)   = fmap (Close x e) ask
evalExpr (App l r)   = do l' <- evalExpr l
                          r' <- evalExpr r
                          applyFun l' r'
evalExpr (Pair l r)  = liftM2 PairV (evalExpr l) (evalExpr r)
evalExpr (Fst e)     = evalExpr e >>= applyFst
evalExpr (Snd e)     = evalExpr e >>= applySnd
evalExpr (Chc p l r) = liftM2 (ChcV p) (evalExpr l) (evalExpr r)

-- | Apply a function to a value.
applyFun :: Value -> Value -> EvalM Value
applyFun (Close x e m) v = local (const (envExtend x v m)) (evalExpr e)
applyFun (ChcV p l r)  v = liftM2 (ChcV p) (applyFun l v) (applyFun r v)
applyFun vl vr = throwError $ "applyFun: type error applying "
                              ++ show vl ++ " to " ++ show vr

-- | Apply a primitive unary function to a value.
applyPrim1 :: MonadError String m => Op1 -> Value -> m Value
applyPrim1 o (Prim v)     = fmap Prim (primOp1 o v)
applyPrim1 o (ChcV p l r) = liftM2 (ChcV p) (applyPrim1 o l) (applyPrim1 o r)
applyPrim1 o v = throwError $ "applyPrim1: type error applying op "
                              ++ show o ++ " to value " ++ show v

-- | Apply a primitive binary function to two values.
applyPrim2 :: MonadError String m => Op2 -> Value -> Value -> m Value
applyPrim2 o (Prim l) (Prim r) = fmap Prim (primOp2 o l r)
applyPrim2 o (ChcV p ll lr) r  = liftM2 (ChcV p) (applyPrim2 o ll r) (applyPrim2 o lr r)
applyPrim2 o l (ChcV p rl rr)  = liftM2 (ChcV p) (applyPrim2 o l rl) (applyPrim2 o l rr)
applyPrim2 o l r = throwError $ "applyPrim2: type error applying op "
                                ++ show o ++ " to value " ++ show l ++ " and " ++ show r

-- | Apply the fst function to a value.
applyFst :: MonadError String m => Value -> m Value
applyFst (PairV l _)  = return l
applyFst (ChcV p l r) = liftM2 (ChcV p) (applyFst l) (applyFst r)
applyFst v = throwError $ "applyFst: type error: " ++ show v

-- | Apply the snd function to a value.
applySnd :: MonadError String m => Value -> m Value
applySnd (PairV _ r)  = return r
applySnd (ChcV p l r) = liftM2 (ChcV p) (applySnd l) (applySnd r)
applySnd v = throwError $ "applySnd: type error: " ++ show v


--
-- * Statement execution
--

-- | Execution monad.
type ExecM = StateT (HEnv Value) EvalM

-- | Execute a statement.
execStmt :: Stmt -> ExecM ()
execStmt = undefined
