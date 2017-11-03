module DSL.Primitive
  (primType
  , opB_B
  , opN_N
  , opF_I
  , opBB_B
  , opNN_B
  , opNN_N
  , Boolean(..)
  , PrimN(..), Prim(..)
  ) where

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
primType PErr  = TBottom

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
