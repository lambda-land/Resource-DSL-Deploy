module DSL.Expression where

import Prelude hiding (LT,GT)

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Control.Monad (zipWithM)
import Control.Monad.Catch (Exception,throwM)

import DSL.Environment
import DSL.Name
import DSL.Path
import DSL.Primitive
import DSL.Resource


--
-- * Expressions
--

-- ** Syntax

-- | Named and primitively typed parameters.
data Param = Param Var PType
  deriving (Data,Eq,Generic,Read,Show,Typeable)

-- | Unary functions.
data Fun = Fun Param Expr
  deriving (Data,Eq,Generic,Read,Show,Typeable)

-- | Expressions.
data Expr
     = Ref Var                 -- ^ variable reference
     | Res Path                -- ^ resource reference
     | Lit PVal                -- ^ primitive literal
     | P1  Op1 Expr            -- ^ primitive unary function
     | P2  Op2 Expr Expr       -- ^ primitive binary function
     | P3  Op3 Expr Expr Expr  -- ^ conditional expression
--     | Chc BExpr Expr Expr     -- ^ choice constructor
  deriving (Data,Eq,Generic,Read,Show,Typeable)

(??) :: Expr -> (Expr,Expr) -> Expr
c ?? (t,e) = P3 Cond c t e

infix 1 ??

-- function to test for ArgTypeError Constructor
isArgTypeError :: ArgTypeError -> Bool
isArgTypeError (ArgTypeError _ _) = True

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
  (./=) = P2 (II_B Neq)
  (.>=) = P2 (II_B GTE)
  (.>)  = P2 (II_B GT)


-- ** Errors

-- | Type error caused by passing argument of the wrong type.
data ArgTypeError = ArgTypeError Param PVal
  deriving (Data,Eq,Generic,Read,Show,Typeable)

instance Exception ArgTypeError


-- ** Semantics

-- | Evaluate an expression.
evalExpr :: MonadEval m => Expr -> m PVal
evalExpr (Ref x)      = getVarEnv >>= envLookup x
evalExpr (Res p)      = do rID <- getResID p
                           env <- getResEnv
                           envLookup rID env
evalExpr (Lit v)      = return v
evalExpr (P1 o e)     = evalExpr e >>= primOp1 o
evalExpr (P2 o l r)   = do l' <- evalExpr l
                           r' <- evalExpr r
                           primOp2 o l' r'
evalExpr (P3 o c t e) = do c' <- evalExpr c
                           t' <- evalExpr t
                           e' <- evalExpr e
                           primOp3 o c' t' e'
-- evalExpr (Chc p l r)  = liftM2 (ChcV p) (evalExpr l) (evalExpr r)

-- | Check the type of an argument.
checkArg :: MonadEval m => Param -> PVal -> m (Var,PVal)
checkArg p@(Param x t) v
    | primType v == t = return (x,v)
    | otherwise       = throwM (ArgTypeError p v)

-- | Evaluate a function.
evalFun :: MonadEval m => Fun -> PVal -> m PVal
evalFun (Fun p@(Param x _) e) v = do
    checkArg p v
    withNewVar x v (evalExpr e)

-- | Run a computation in a variable environment extended by new arguments.
withArgs :: MonadEval m => [Param] -> [Expr] -> m a -> m a
withArgs xs args go = do
    vals <- mapM evalExpr args
    new  <- fmap envFromList (zipWithM checkArg xs vals)
    withVarEnv (envUnion new) go
