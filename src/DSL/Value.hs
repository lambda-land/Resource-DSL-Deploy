module DSL.Value where

import DSL.Types
import DSL.Primitive
import DSL.Resource

applyPrim1 :: MonadEval m => Op1 -> VM m PVal -> VM m PVal
applyPrim1 o v = do
  p <- v
  toVM $ promoteError (primOp1 o p)

applyPrim2 :: MonadEval m => Op2 -> VM m PVal -> VM m PVal -> VM m PVal
applyPrim2 o v1 v2 = do
  p1 <- v1
  p2 <- v2
  toVM $ promoteError (primOp2 o p1 p2)

applyPrim3 :: MonadEval m => Op3 -> VM m PVal -> VM m PVal -> VM m PVal -> VM m PVal
applyPrim3 o v1 v2 v3 = do
  p1 <- v1
  p2 <- v2
  p3 <- v3
  toVM $ promoteError (primOp3 o p1 p2 p3)
