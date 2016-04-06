{-# LANGUAGE
      DeriveGeneric,
      MultiParamTypeClasses
  #-}

module DSL.Expr where

import Prelude hiding (LT,GT)

import Data.String (IsString)
import GHC.Generics (Generic)

import DSL.Env
import DSL.Primitive
import DSL.Row
import DSL.Type


--
-- * Expression Syntax
--

-- | Expressions.
data Expr t
     -- literals and primitives
     = Unit                              -- ^ unit value
     | B Bool                            -- ^ boolean literal
     | I Int                             -- ^ integer literal
     -- primitive functions
     | P1 Op1 (Expr t)                   -- ^ primitive unary function
     | P2 Op2 (Expr t) (Expr t)          -- ^ primitive binary function
     -- simply typed lambda calculus
     | Ref Var                           -- ^ non-linear variable reference
     | Use Var                           -- ^ linear variable reference
     | Fun Var (Schema t) (Expr t)       -- ^ lambda abstraction
     | App (Expr t) (Expr t)             -- ^ application
     -- products
     | Pair (Expr t) (Expr t)            -- ^ construct product type
     | Both (Expr t) (Var,Var) (Expr t)  -- ^ consume product type
     -- reuse
     | Free (Expr t)                     -- ^ mark reusable term
     | Reuse (Expr t) Var (Expr t)       -- ^ use reusable term
     | Waste (Expr t) (Expr t)           -- ^ consume a term
     -- records
     | Rec (Row (Expr t))                -- ^ record values
     | Sel Label (Expr t)                -- ^ record selection
     | Ext Label (Expr t) (Expr t)       -- ^ record extension
  deriving (Eq,Generic,Show)

-- | Is this term in normal form?
isNormal :: Expr t -> Bool
isNormal Unit        = True
isNormal (B _)       = True
isNormal (I _)       = True
isNormal (Ref _)     = True
isNormal (Use _)     = True
isNormal (Fun x _ _) = True   -- don't normalize under abstraction
isNormal (Free e)    = isNormal e
isNormal (Pair l r)  = isNormal l && isNormal r
isNormal (Rec r)     = all isNormal r
isNormal _           = False


-- ** Syntactic sugar

-- | Binary function application.
app2 :: Expr t -> Expr t -> Expr t -> Expr t
app2 f x y = App (App f x) y

-- | Build record from association list.
rec :: [(Label, Expr t)] -> Expr t
rec = Rec . row

-- | Delete record entry.
recDelete :: Label -> Expr t -> Expr t
recDelete l e = Both (Sel l e) ("v","r") (Waste (Use "v") (Use "r"))

-- | Update record entry.
recUpdate :: Label -> Expr t -> Expr t -> Expr t
recUpdate l v e = Ext l v (recDelete l e)

-- Use SBV's Boolean type class for boolean predicates.
instance Boolean (Expr t) where
  true  = B True
  false = B False
  bnot  = P1 (B_B Not)
  (&&&) = P2 (BB_B And)
  (|||) = P2 (BB_B Or)
  (<+>) = P2 (BB_B XOr)
  (==>) = P2 (BB_B Imp)

-- Use Num type class for integer arithmetic.
instance Num (Expr t) where
  fromInteger = I . fromInteger
  abs    = P1 (I_I Abs)
  negate = P1 (I_I Neg)
  signum = P1 (I_I Sign)
  (+)    = P2 (II_I Add)
  (-)    = P2 (II_I Sub)
  (*)    = P2 (II_I Mul)
  
-- Other integer arithmetic primitives.
instance PrimI (Expr t) where
  (./) = P2 (II_I Div)
  (.%) = P2 (II_I Mod)

-- Integer comparison primitives.
instance Prim (Expr t) (Expr t) where
  (.<)  = P2 (II_B LT)
  (.<=) = P2 (II_B LTE)
  (.==) = P2 (II_B Equ)
  (.>=) = P2 (II_B GTE)
  (.>)  = P2 (II_B GT)
