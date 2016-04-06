
module DSL.Semantics where

import Control.Monad

import DSL.Env
import DSL.Expr
import DSL.Primitive
import DSL.Row

-- 
-- * Evaluation
--

-- | Run the evaluation function starting with an empty environment.
--   Returns either the evaluated result or an error message.
runEval :: Show t => Expr t -> Either String (Expr t)
runEval = runEnv . evalExpr

-- | Evaluate an expression in the IO monad.
runEvalIO :: Show t => Expr t -> IO (Expr t)
runEvalIO = either fail return . runEval

-- | Evaluate an expression.
evalExpr :: Show t => Expr t -> EnvM (Expr t) (Expr t)
  -- lambda calculus
evalExpr (Ref x) = lookRef x
evalExpr (Use x) = lookUse x
evalExpr (App l r) = do 
    l' <- evalExpr l
    r' <- evalExpr r
    case l' of
      Fun x _ e -> addLinear x r' (evalExpr e)
      _ -> fail (expectMsg "abstraction" l' ++ afterMsg l)
  -- primitives
evalExpr ctx@(P1 (B_B o) e) = fmap (B . opB_B o) (evalBool e ctx)
evalExpr ctx@(P1 (I_I o) e) = fmap (I . opI_I o) (evalInt e ctx)
evalExpr ctx@(P2 (BB_B o) l r) = fmap B (liftM2 (opBB_B o) (evalBool l ctx) (evalBool r ctx))
evalExpr ctx@(P2 (II_I o) l r) = fmap I (liftM2 (opII_I o) (evalInt l ctx) (evalInt r ctx))
evalExpr ctx@(P2 (II_B o) l r) = fmap B (liftM2 (opII_B o) (evalInt l ctx) (evalInt r ctx))
  -- pairs
evalExpr (Pair l r) = liftM2 Pair (evalExpr l) (evalExpr r)
evalExpr (Both e (x,y) body) = do
    e' <- evalExpr e
    case e' of
      Pair l r -> addLinear x l (addLinear y r (evalExpr body))
      _ -> fail (expectMsg "pair" e' ++ afterMsg e)
  -- reuse
evalExpr (Free e) = fmap Free (evalExpr e)
evalExpr (Reuse e x body) = do
    e' <- evalExpr e
    case e' of
      Free e'' -> addLocal x e'' (evalExpr body)
      _ -> fail (expectMsg "free expression" e' ++ afterMsg e)
evalExpr (Waste e body) = evalExpr e >> evalExpr body
  -- records
evalExpr (Rec r) = fmap Rec (traverse evalExpr r)
evalExpr (Sel l e) = do
    e' <- evalExpr e
    case e' of
      Rec r -> case rowExtract l r of
                 Just (v,r') -> return (Pair v (Rec r'))
                 _ -> fail (expectMsg ("label " ++ l) (Rec r))
      _ -> fail (expectMsg "record" e' ++ afterMsg e)
evalExpr (Ext l v e) = do
    v' <- evalExpr v
    e' <- evalExpr e
    case e' of
      Rec r -> if hasLabel l r
                 then fail (expectMsg ("no label" ++ l) (Rec r))
                 else return (Rec (rowExtend l v' r))
      _ -> fail (expectMsg "record" e' ++ afterMsg e)
  -- values
evalExpr e = unless (isNormal e) (fail msg) >> return e
  where msg = expectMsg "normalized value" e

-- | Evaluate an expression to a boolean to apply to a primitive operation.
evalBool :: Show t => Expr t -> Expr t -> EnvM (Expr t) Bool
evalBool e ctx = do
    e' <- evalExpr e
    case e' of
      B b -> return b
      _ -> fail (expectMsg "boolean" e' ++ afterMsg e ++ inCtxMsg ctx)

-- | Evaluate an expression to an integer to apply to a primitive operation.
evalInt :: Show t => Expr t -> Expr t -> EnvM (Expr t) Int
evalInt e ctx = do
    e' <- evalExpr e
    case e' of
      I i -> return i
      _ -> fail (expectMsg "integer" e' ++ afterMsg e ++ inCtxMsg ctx)

-- | Error message helper.
expectMsg :: Show t => String -> Expr t -> String
expectMsg s e = "evalExpr: expected " ++ s ++ ", got:\n" ++ show e

-- | Error message helper.
afterMsg :: Show t => Expr t -> String
afterMsg e = "\nafter reducing:\n" ++ show e

-- | Error message helper.
inCtxMsg :: Show t => Expr t -> String
inCtxMsg e = "\nin the context of:\n" ++ show e
