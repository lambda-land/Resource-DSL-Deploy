module DSL.Semantics where

import Control.Monad.Except (throwError)
import Control.Monad (liftM2)
import Control.Monad.Reader

import DSL.Environment
import DSL.Syntax
import DSL.Value


--
-- * Expression evaluation
--

-- | Expression evaluation monad.
type ExprM = ReaderT (Value, Env Value) (Either String)

-- | Evaluate an expression.
evalExpr :: Expr -> ExprM Value
evalExpr This        = reader fst
evalExpr (Ref x)     = reader snd >>= envLookup x
evalExpr (Let x b e) = do b' <- evalExpr b
                          local (\(t,m) -> (t, envExtend x b' m)) (evalExpr e)
evalExpr (Lit v)     = return (Lit_ v)
evalExpr (P1 o e)    = evalExpr e >>= applyPrim1 o
evalExpr (P2 o l r)  = do l' <- evalExpr l
                          r' <- evalExpr r
                          applyPrim2 o l' r'
evalExpr (Tup l r)   = liftM2 Tup_ (evalExpr l) (evalExpr r)
evalExpr (Fst e)     = evalExpr e >>= applyFst
evalExpr (Snd e)     = evalExpr e >>= applySnd
evalExpr (Chc p l r) = liftM2 (Chc_ p) (evalExpr l) (evalExpr r)
