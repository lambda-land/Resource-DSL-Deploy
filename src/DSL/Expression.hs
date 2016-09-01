module DSL.Expression where

import Prelude hiding (LT,GT)

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Control.Monad (liftM2)

import DSL.Environment
import DSL.Predicate
import DSL.Primitive
import DSL.Resource
import DSL.Value


--
-- * Expressions
--

-- ** Syntax

-- | Expressions.
data Expr
     = Ref Var              -- ^ variable reference
     | Lit PVal             -- ^ primitive literal
     | P1  Op1 Expr         -- ^ primitive unary function
     | P2  Op2 Expr Expr    -- ^ primitive binary function
     | Chc BExpr Expr Expr  -- ^ choice constructor
  deriving (Data,Eq,Generic,Read,Show,Typeable)

-- | Unary functions.
type Fun = (Var,Expr)

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


-- ** Semantics

-- | Evaluate an expression.
evalExpr :: MonadEval m => Expr -> m Value
evalExpr (Ref x)     = getVarEnv >>= envLookup x
evalExpr (Lit v)     = return (Prim v)
evalExpr (P1 o e)    = evalExpr e >>= applyPrim1 o
evalExpr (P2 o l r)  = do l' <- evalExpr l
                          r' <- evalExpr r
                          applyPrim2 o l' r'
evalExpr (Chc p l r) = liftM2 (ChcV p) (evalExpr l) (evalExpr r)

-- | Evaluate a function.
evalFun :: MonadEval m => Fun -> Value -> m Value
evalFun (x,e) v = withVarEnv (envExtend x v) (evalExpr e)
