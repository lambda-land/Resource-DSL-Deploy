{-# LANGUAGE
      DeriveGeneric,
      MultiParamTypeClasses
  #-}

module DSL.Syntax where

import Prelude hiding (LT,GT)

import Data.String (IsString)
import GHC.Generics (Generic)

import DSL.Environment
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
     = Do Cmd Name Expr     -- ^ apply a command to a resource
     | In Path Block        -- ^ do work in a sub-environment
     | If Pred Block Block  -- ^ conditional statement
  deriving (Eq,Generic,Show)

-- | Resource commands:
--    * check whether a resource satisfies a given predicate
--    * modify an existing resource
--    * provide a new resource
data Cmd = Check | Modify | Provide
  deriving (Eq,Generic,Show)

-- | Expressions.
data Expr
     -- naming
     = This                -- ^ reference to the current resource
     | Ref Var             -- ^ variable reference
     | Let Var Expr Expr   -- ^ local variable declaration
     -- primitives
     | Lit PVal            -- ^ primitive literals
     | P1  Op1 Expr        -- ^ primitive unary function
     | P2  Op2 Expr Expr   -- ^ primitive binary function
     -- pairs
     | Tup Expr Expr       -- ^ pair constructor
     | Fst Expr            -- ^ first item from pair
     | Snd Expr            -- ^ second item from pair
     -- variation
     | Chc Pred Expr Expr  -- ^ choice constructor
  deriving (Eq,Generic,Show)


-- ** Syntactic sugar

-- | Check whether a resource satisfies a given predicate.
check :: Name -> Expr -> Stmt
check = Do Check

-- | Modify an existing resource.
modify :: Name -> Expr -> Stmt
modify = Do Modify

-- | Provide a new resource.
provide :: Name -> Expr -> Stmt
provide = Do Provide

-- | Require that a unit valued resource is present.
require :: Name -> Stmt
require r = check r (P1 IsU This)

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
