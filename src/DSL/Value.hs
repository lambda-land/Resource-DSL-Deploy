module DSL.Value where

import DSL.Types
import DSL.Primitive
import DSL.Resource

-- | Evaluate a primitive unary operator.
primOp1 :: MonadEval m => Op1 -> PVal -> m PVal
primOp1 U_U     Unit  = return Unit
primOp1 (B_B o) (B b) = return (B (opB_B o b))
primOp1 (N_N o) (I n) = return (I (opN_N o n))
primOp1 (N_N o) (F n) = return (F (opN_N o n))
primOp1 (F_I o) (F n) = return (I (opF_I o n))
primOp1 o v = vError (PrimE $ ErrorOp1 o v)
-- | Evaluate a primitive binary operator. When a binary numeric operator is
--   applied to one integer and one floating point number, the integer is
--   implicitly converted to floating point.
primOp2 :: MonadEval m => Op2 -> PVal -> PVal -> m PVal
  -- boolean
primOp2 (BB_B o) (B l) (B r) = return (B (opBB_B o l r))
  -- arithmetic
primOp2 (NN_N o) (I l) (I r) = return (I (opNN_N o l r))
primOp2 (NN_N o) (I l) (F r) = return (F (opNN_N o (fromIntegral l) r))
primOp2 (NN_N o) (F l) (I r) = return (F (opNN_N o l (fromIntegral r)))
primOp2 (NN_N o) (F l) (F r) = return (F (opNN_N o l r))
  -- comparison
primOp2 (NN_B o) (I l) (I r) = return (B (opNN_B o l r))
primOp2 (NN_B o) (I l) (F r) = return (B (opNN_B o (fromIntegral l) r))
primOp2 (NN_B o) (F l) (I r) = return (B (opNN_B o l (fromIntegral r)))
primOp2 (NN_B o) (F l) (F r) = return (B (opNN_B o l r))
  -- error
primOp2 o l r = vError (PrimE $ ErrorOp2 o l r)

-- | Evaluate a primitive ternary operator.
primOp3 :: MonadEval m => Op3 -> PVal -> PVal -> PVal -> m PVal
primOp3 Cond (B c) t e = return (if c then t else e)
primOp3 o c t e = vError (PrimE $ ErrorOp3 o c t e)
