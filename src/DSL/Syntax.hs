{-# LANGUAGE
      DeriveGeneric,
      MultiParamTypeClasses
  #-}

module DSL.Syntax where

import Prelude hiding (LT,GT)

import Data.String (IsString)
import GHC.Generics (Generic)

import DSL.Env
import DSL.Primitive
import DSL.Predicate


--
-- * Syntax
--

-- | A named and typed parameter.
type Param = (Var,PType)

-- | A statement block.
type Block = [Stmt]

-- | Resource description: a parameterized specification of the resource
--   requirements and provisions of a component (DFU).
data Desc = Desc [Param] Block
  deriving (Eq,Generic,Show)

-- | Statements in a resource description.
data Stmt
     = Cmd Name Cmd          -- ^ apply a command to a resource
     | In  Path Block        -- ^ do work in a sub-environment
     | If  Pred Block Block  -- ^ conditional statement
  deriving (Eq,Generic,Show)

-- | Resource commands.
data Cmd
     = Chk                   -- ^ check whether a unit resource is present
     | Sat Var Pred          -- ^ check whether a resource satisfies the given predicate
     | Use Var Expr          -- ^ modify an existing resource
     | New Expr              -- ^ provide a new resource
  deriving (Eq,Generic,Show)

-- | Expressions.
data Expr
     = Lit PVal              -- ^ literals
     | Ref Var               -- ^ variable reference
     | P1  Op1 Expr          -- ^ primitive unary function
     | P2  Op2 Expr Expr     -- ^ primitive binary function
     | Chc Pred Expr Expr    -- ^ choice
  deriving (Eq,Generic,Show)

-- | Values.
data Val
     = VLit PVal
     | VChc Pred Val Val
  deriving (Eq,Show)



-- ** Syntactic sugar

-- Use SBV's Boolean type class for boolean predicates.
instance Boolean Expr where
  true  = Lit (B True)
  false = Lit (B False)
  bnot  = P1 (B_B Not)
  (&&&) = P2 (BB_B And)
  (|||) = P2 (BB_B Or)
  (<+>) = P2 (BB_B XOr)
  (==>) = P2 (BB_B Imp)
  (<=>) = P2 (BB_B Eqv)

-- Use Num type class for integer arithmetic.
instance Num Expr where
  fromInteger = Lit . I . fromInteger
  abs    = P1 (I_I Abs)
  negate = P1 (I_I Neg)
  signum = P1 (I_I Sign)
  (+)    = P2 (II_I Add)
  (-)    = P2 (II_I Sub)
  (*)    = P2 (II_I Mul)
  
-- Other integer arithmetic primitives.
instance PrimI Expr where
  (./) = P2 (II_I Div)
  (.%) = P2 (II_I Mod)

-- Integer comparison primitives.
instance Prim Expr Expr where
  (.<)  = P2 (II_B LT)
  (.<=) = P2 (II_B LTE)
  (.==) = P2 (II_B Equ)
  (.>=) = P2 (II_B GTE)
  (.>)  = P2 (II_B GT)
