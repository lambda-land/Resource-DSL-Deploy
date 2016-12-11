module DSL.Primitive 
  ( PType(..), PVal(..)
  , primType
  , Op1(..), Op2(..), Op3(..)
  , primOp1, primOp2, primOp3
  , B_B(..), opB_B
  , N_N(..), opN_N
  , F_I(..), opF_I
  , BB_B(..), opBB_B
  , NN_B(..), opNN_B
  , NN_N(..), opNN_N
  , Boolean(..)
  , PrimN(..), Prim(..)
  ) where

import Prelude hiding (LT,GT)

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Control.Monad.Catch (Exception,MonadThrow,throwM)

import Data.Fixed (mod')
import Data.SBV (Boolean(..),SBool,SInteger,SInt8,SInt16,SInt32,SInt64)
import qualified Data.SBV as SBV

import DSL.Name


--
-- * Base types and values
--

-- | Symbols.

-- | Primitive base types.
data PType = TUnit | TBool | TInt | TFloat | TSymbol
  deriving (Data,Eq,Generic,Read,Show,Typeable)

-- | Primitive values.
data PVal
     = Unit
     | B Bool
     | I Int
     | F Double
     | S Symbol
  deriving (Data,Eq,Generic,Read,Show,Typeable)

-- | Type of primitive value.
primType :: PVal -> PType
primType Unit  = TUnit
primType (B _) = TBool
primType (I _) = TInt
primType (F _) = TFloat
primType (S _) = TSymbol


--
-- * Primitive operators
--

-- | Primitive unary operators organized by type.
data Op1
     = U_U        -- ^ noop that matches a unit value
     | B_B B_B    -- ^ unary boolean operator
     | N_N N_N    -- ^ unary numeric operator
     | F_I F_I    -- ^ unary float-to-integer operator
  deriving (Data,Eq,Generic,Read,Show,Typeable)

-- | Primitive binary operators organized by type.
data Op2
     = BB_B BB_B  -- ^ binary boolean operator
     | NN_N NN_N  -- ^ binary numeric operator
     | NN_B NN_B  -- ^ numeric comparison operator
  deriving (Data,Eq,Generic,Read,Show,Typeable)

-- | Primitive ternary operator.
data Op3 = Cond
  deriving (Data,Eq,Generic,Read,Show,Typeable)

-- | Boolean negation.
data B_B = Not
  deriving (Data,Eq,Generic,Read,Show,Typeable)

-- | Unary numeric operators.
data N_N = Abs | Neg | Sign
  deriving (Data,Eq,Generic,Read,Show,Typeable)

-- | Unary float-to-integer operators.
data F_I = Ceil | Floor | Round
  deriving (Data,Eq,Generic,Read,Show,Typeable)

-- | Binary boolean operators.
data BB_B = And | Or | XOr | Imp | Eqv
  deriving (Data,Eq,Generic,Read,Show,Typeable)

-- | Binary numeric comparison operators.
data NN_B = LT | LTE | Equ | Neq | GTE | GT
  deriving (Data,Eq,Generic,Read,Show,Typeable)

-- | Binary numeric arithmetic operators.
data NN_N = Add | Sub | Mul | Div | Mod
  deriving (Data,Eq,Generic,Read,Show,Typeable)

-- | Type error applying primitive operator.
data PrimTypeError
     = ErrorOp1 Op1 PVal
     | ErrorOp2 Op2 PVal PVal
     | ErrorOp3 Op3 PVal PVal PVal
  deriving (Data,Eq,Generic,Read,Show,Typeable)

instance Exception PrimTypeError

-- | Evaluate a primitive unary operator.
primOp1 :: MonadThrow m => Op1 -> PVal -> m PVal
primOp1 U_U     Unit  = return Unit
primOp1 (B_B o) (B b) = return (B (opB_B o b))
primOp1 (N_N o) (I n) = return (I (opN_N o n))
primOp1 (N_N o) (F n) = return (F (opN_N o n))
primOp1 (F_I o) (F n) = return (I (opF_I o n))
primOp1 o v = throwM (ErrorOp1 o v)

-- | Evaluate a primitive binary operator. When a binary numeric operator is
--   applied to one integer and one floating point number, the integer is
--   implicitly converted to floating point.
primOp2 :: MonadThrow m => Op2 -> PVal -> PVal -> m PVal
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
primOp2 o l r = throwM (ErrorOp2 o l r)

-- | Evaluate a primitive ternary operator.
primOp3 :: MonadThrow m => Op3 -> PVal -> PVal -> PVal -> m PVal
primOp3 Cond (B c) t e = return (if c then t else e)
primOp3 o c t e = throwM (ErrorOp3 o c t e)

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

-- | Add division and modulus to the Num type class.
class Num n => PrimN n where
  (./), (.%) :: n -> n -> n

-- | A type class for overloading the primitive operators.
class (Boolean b, PrimN n) => Prim b n where
  (.<), (.<=), (.==), (./=), (.>=), (.>) :: n -> n -> b

infix 4 .<, .<=, .==, ./=, .>=, .>
infixl 7 ./, .%


-- Ground instances

instance PrimN Int where
  (./) = div
  (.%) = mod

instance PrimN Double where
  (./) = (/)
  (.%) = mod'

instance Prim Bool Int where
  (.<)  = (<)
  (.<=) = (<=)
  (.==) = (==)
  (./=) = (/=)
  (.>=) = (>=)
  (.>)  = (>)

instance Prim Bool Double where
  (.<)  = (<)
  (.<=) = (<=)
  (.==) = (==)
  (./=) = (/=)
  (.>=) = (>=)
  (.>)  = (>)


-- Symbolic instances

instance PrimN SInteger where
  (./)  = SBV.sDiv
  (.%)  = SBV.sMod

instance PrimN SInt8 where
  (./)  = SBV.sDiv
  (.%)  = SBV.sMod

instance PrimN SInt16 where
  (./)  = SBV.sDiv
  (.%)  = SBV.sMod

instance PrimN SInt32 where
  (./)  = SBV.sDiv
  (.%)  = SBV.sMod

instance PrimN SInt64 where
  (./)  = SBV.sDiv
  (.%)  = SBV.sMod

instance Prim SBool SInteger where
  (.<)  = (SBV..<)
  (.<=) = (SBV..<=)
  (.==) = (SBV..==)
  (./=) = (SBV../=)
  (.>=) = (SBV..>=)
  (.>)  = (SBV..>)

instance Prim SBool SInt8 where
  (.<)  = (SBV..<)
  (.<=) = (SBV..<=)
  (.==) = (SBV..==)
  (./=) = (SBV../=)
  (.>=) = (SBV..>=)
  (.>)  = (SBV..>)

instance Prim SBool SInt16 where
  (.<)  = (SBV..<)
  (.<=) = (SBV..<=)
  (.==) = (SBV..==)
  (./=) = (SBV../=)
  (.>=) = (SBV..>=)
  (.>)  = (SBV..>)

instance Prim SBool SInt32 where
  (.<)  = (SBV..<)
  (.<=) = (SBV..<=)
  (.==) = (SBV..==)
  (./=) = (SBV../=)
  (.>=) = (SBV..>=)
  (.>)  = (SBV..>)

instance Prim SBool SInt64 where
  (.<)  = (SBV..<)
  (.<=) = (SBV..<=)
  (.==) = (SBV..==)
  (./=) = (SBV../=)
  (.>=) = (SBV..>=)
  (.>)  = (SBV..>)
