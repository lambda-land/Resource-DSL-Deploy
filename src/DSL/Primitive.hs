module DSL.Primitive where

import Prelude hiding (LT,GT)

import Data.SBV (Boolean(..))
import DSL.Types

--
-- * Base types and values
--

-- | Type of primitive value.
primType :: PVal -> PType
primType Unit  = TUnit
primType (B _) = TBool
primType (I _) = TInt
primType (F _) = TFloat
primType (S _) = TSymbol

-- | Evaluate a primitive unary operator.
primOp1 :: Op1 -> PVal -> Either Error PVal
primOp1 U_U     Unit  = Right Unit
primOp1 (B_B o) (B b) = Right (B (opB_B o b))
primOp1 (N_N o) (I n) = Right (I (opN_N o n))
primOp1 (N_N o) (F n) = Right (F (opN_N o n))
primOp1 (F_I o) (F n) = Right (I (opF_I o n))
-- Primitive type mismatch
primOp1 o v = Left (PrimE $ ErrorOp1 o v)

-- | Evaluate a primitive binary operator. When a binary numeric operator is
--   applied to one integer and one floating point number, the integer is
--   implicitly converted to floating point.
primOp2 :: Op2 -> PVal -> PVal -> Either Error PVal
  -- boolean
primOp2 (BB_B o) (B l) (B r) = Right (B (opBB_B o l r))
  -- arithmetic
primOp2 (NN_N o) (I l) (I r) = Right (I (opNN_N o l r))
primOp2 (NN_N o) (I l) (F r) = Right (F (opNN_N o (fromIntegral l) r))
primOp2 (NN_N o) (F l) (I r) = Right (F (opNN_N o l (fromIntegral r)))
primOp2 (NN_N o) (F l) (F r) = Right (F (opNN_N o l r))
  -- comparison
primOp2 (NN_B o) (I l) (I r) = Right (B (opNN_B o l r))
primOp2 (NN_B o) (I l) (F r) = Right (B (opNN_B o (fromIntegral l) r))
primOp2 (NN_B o) (F l) (I r) = Right (B (opNN_B o l (fromIntegral r)))
primOp2 (NN_B o) (F l) (F r) = Right (B (opNN_B o l r))
primOp2 (SS_B SEqu) (S s) (S s') = Right (B (s==s'))
  -- error
primOp2 o l r = Left (PrimE $ ErrorOp2 o l r)

-- | Evaluate a primitive ternary operator.
primOp3 :: Op3 -> PVal -> PVal -> PVal -> Either Error PVal
primOp3 Cond (B c) t e = Right (if c then t else e)
primOp3 o c t e = Left (PrimE $ ErrorOp3 o c t e)

-- | Lookup unary boolean operator.
opB_B :: Boolean b => B_B -> b -> b
opB_B Not = bnot

-- | Lookup unary integer operator.
opN_N :: Num n => N_N -> n -> n
opN_N Abs  = abs
opN_N Neg  = negate
opN_N Sign = signum

-- | Lookup unary float-to-integer operator.
opF_I :: (RealFrac f, Integral i) => F_I -> f -> i
opF_I Ceil  = ceiling
opF_I Floor = floor
opF_I Round = round

-- | Lookup binary boolean operator.
opBB_B :: Boolean b => BB_B -> b -> b -> b
opBB_B And = (&&&)
opBB_B Or  = (|||)
opBB_B XOr = (<+>)
opBB_B Imp = (==>)
opBB_B Eqv = (<=>)

-- | Lookup binary integer comparison operator.
opNN_B :: Prim b n => NN_B -> n -> n -> b
opNN_B LT  = (.<)
opNN_B LTE = (.<=)
opNN_B Equ = (.==)
opNN_B Neq = (./=)
opNN_B GTE = (.>=)
opNN_B GT  = (.>)

-- | Lookup binary integer operator.
opNN_N :: PrimN n => NN_N -> n -> n -> n
opNN_N Add = (+)
opNN_N Sub = (-)
opNN_N Mul = (*)
opNN_N Div = (./)
opNN_N Mod = (.%)
