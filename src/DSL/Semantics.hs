
module DSL.Semantics where

import Control.Monad

import DSL.Env
import DSL.Row
import DSL.Expr

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

-- | Error message helper.
expectMsg :: Show t => String -> Expr t -> String
expectMsg s e = "evalExpr: expected " ++ s ++ ", got:\n" ++ show e

-- | Error message helper.
afterMsg :: Show t => Expr t -> String
afterMsg e = "after reducing:\n" ++ show e
