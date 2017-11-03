module DSL.Value where

import DSL.Types
import DSL.Primitive
import DSL.Resource


applyPrim1 :: MonadEval m => Op1 -> m Value -> m Value
applyPrim1 o v = vBind v (\p -> promoteError (primOp1 o p) >>= vReturn)

applyPrim2 :: MonadEval m => Op2 -> m Value -> m Value -> m Value
applyPrim2 o v1 v2 = vBind v1 (\p1 ->
                                vBind v2 (\p2 ->
                                  promoteError (primOp2 o p1 p2) >>= vReturn))

applyPrim3 :: MonadEval m => Op3 -> m Value -> m Value -> m Value -> m Value
applyPrim3 o v1 v2 v3 = vBind v1 (\p1 ->
                                   vBind v2 (\p2 ->
                                     vBind v3 (\p3 ->
                                       promoteError (primOp3 o p1 p2 p3) >>= vReturn)))
