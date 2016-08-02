{-# LANGUAGE
      DeriveGeneric,
      FlexibleContexts,
      FlexibleInstances,
      MultiParamTypeClasses
  #-}

module DSL.Primitive 
  ( PType(..), PVal(..)
  , Op1(..), Op2(..)
  , primOp1, primOp2
  , B_B(..), opB_B
  , I_I(..), opI_I
  , BB_B(..), opBB_B
  , II_B(..), opII_B
  , II_I(..), opII_I
  , Boolean(..)
  , PrimI(..), Prim(..)
  ) where

import Prelude hiding (LT,GT)

import Control.Monad.Except (MonadError,throwError)

import Data.Bits
import Data.SBV (Boolean(..),SBool,SInteger,SInt8,SInt16,SInt32,SInt64)
import qualified Data.SBV as SBV

import GHC.Generics (Generic)


--
-- * Base types and values
--

-- | Primitive base types.
data PType = TUnit | TBool | TInt
  deriving (Eq,Generic,Show)

-- | Primitive values.
data PVal
     = Unit
     | B Bool
     | I Int
  deriving (Eq,Generic,Show)


--
-- * Primitive operators
--

-- | Primitive unary operators organized by type.
data Op1
     = IsU        -- ^ noop that matches a unit value
     | B_B B_B    -- ^ unary boolean operation
     | I_I I_I    -- ^ unary integer operation
  deriving (Eq,Generic,Show)

-- | Primitive binary operators organized by type.
data Op2
     = BB_B BB_B  -- ^ binary boolean operator
     | II_I II_I  -- ^ binary integer operator
     | II_B II_B  -- ^ integer comparison operator
  deriving (Eq,Generic,Show)

-- | Boolean negation.
data B_B = Not
  deriving (Eq,Generic,Show)

-- | Unary integer operators.
data I_I = Abs | Neg | Sign
  deriving (Eq,Generic,Show)

-- | Binary boolean operators.
data BB_B = And | Or | XOr | Imp | Eqv
  deriving (Eq,Generic,Show)

-- | Binary integer comparison operators.
data II_B = LT | LTE | Equ | GTE | GT
  deriving (Eq,Generic,Show)

-- | Binary integer arithmetic operators.
data II_I = Add | Sub | Mul | Div | Mod
  deriving (Eq,Generic,Show)

-- | Evaluate a primitive unary operator.
primOp1 :: MonadError String m => Op1 -> PVal -> m PVal
primOp1 IsU     Unit  = return Unit
primOp1 (B_B o) (B b) = return (B (opB_B o b))
primOp1 (I_I o) (I i) = return (I (opI_I o i))
primOp1 o e = throwError $ "primOp1: type error applying operator "
                           ++ show o ++ " to " ++ show e

-- | Evaluate a primitive binary operator.
primOp2 :: MonadError String m => Op2 -> PVal -> PVal -> m PVal
primOp2 (BB_B o) (B l) (B r) = return (B (opBB_B o l r))
primOp2 (II_I o) (I l) (I r) = return (I (opII_I o l r))
primOp2 (II_B o) (I l) (I r) = return (B (opII_B o l r))
primOp2 o l r = throwError $ "primOp2: type error applying operator "
                             ++ show o ++ " to " ++ show l ++ " and " ++ show r

-- | Lookup unary boolean operator.
opB_B :: Boolean b => B_B -> b -> b
opB_B Not = bnot

-- | Lookup unary integer operator.
opI_I :: Num i => I_I -> i -> i
opI_I Abs  = abs
opI_I Neg  = negate
opI_I Sign = signum

-- | Lookup binary boolean operator.
opBB_B :: Boolean b => BB_B -> b -> b -> b
opBB_B And = (&&&)
opBB_B Or  = (|||)
opBB_B XOr = (<+>)
opBB_B Imp = (==>)
opBB_B Eqv = (<=>)

-- | Lookup binary integer comparison operator.
opII_B :: Prim b i => II_B -> i -> i -> b
opII_B LT  = (.<)
opII_B LTE = (.<=)
opII_B Equ = (.==)
opII_B GTE = (.>=)
opII_B GT  = (.>)

-- | Lookup binary integer operator.
opII_I :: PrimI i => II_I -> i -> i -> i
opII_I Add = (+)
opII_I Sub = (-)
opII_I Mul = (*)
opII_I Div = (./)
opII_I Mod = (.%)

-- | Add division and modulus to the Num type class.
class Num i => PrimI i where
  (./), (.%) :: i -> i -> i

-- | A type class for overloading the primitive operators.
class (Boolean b, PrimI i) => Prim b i where
  (.<), (.<=), (.==), (.>=), (.>) :: i -> i -> b

infix 4 .<, .<=, .==, .>=, .>
infixl 7 ./, .%

-- Ground instances

instance PrimI Int where
  (./)  = div
  (.%)  = mod

instance Prim Bool Int where
  (.<)  = (<)
  (.<=) = (<=)
  (.==) = (==)
  (.>=) = (>=)
  (.>)  = (>)

-- Symbolic instances

instance PrimI SInteger where
  (./)  = SBV.sDiv
  (.%)  = SBV.sMod

instance PrimI SInt8 where
  (./)  = SBV.sDiv
  (.%)  = SBV.sMod

instance PrimI SInt16 where
  (./)  = SBV.sDiv
  (.%)  = SBV.sMod

instance PrimI SInt32 where
  (./)  = SBV.sDiv
  (.%)  = SBV.sMod

instance PrimI SInt64 where
  (./)  = SBV.sDiv
  (.%)  = SBV.sMod

instance Prim SBool SInteger where
  (.<)  = (SBV..<)
  (.<=) = (SBV..<=)
  (.==) = (SBV..==)
  (.>=) = (SBV..>=)
  (.>)  = (SBV..>)

instance Prim SBool SInt8 where
  (.<)  = (SBV..<)
  (.<=) = (SBV..<=)
  (.==) = (SBV..==)
  (.>=) = (SBV..>=)
  (.>)  = (SBV..>)

instance Prim SBool SInt16 where
  (.<)  = (SBV..<)
  (.<=) = (SBV..<=)
  (.==) = (SBV..==)
  (.>=) = (SBV..>=)
  (.>)  = (SBV..>)

instance Prim SBool SInt32 where
  (.<)  = (SBV..<)
  (.<=) = (SBV..<=)
  (.==) = (SBV..==)
  (.>=) = (SBV..>=)
  (.>)  = (SBV..>)

instance Prim SBool SInt64 where
  (.<)  = (SBV..<)
  (.<=) = (SBV..<=)
  (.==) = (SBV..==)
  (.>=) = (SBV..>=)
  (.>)  = (SBV..>)
