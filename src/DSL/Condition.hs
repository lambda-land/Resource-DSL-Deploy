module DSL.Condition where

import Prelude hiding (LT,GT)

import Data.Function (on)
import Data.Set (Set)
import qualified Data.Set as Set
import Z3.Monad

import DSL.Boolean
import DSL.Types
import DSL.Environment
import DSL.Primitive
import DSL.SAT


--
-- * Conditions
--

-- ** Operations

-- | The true condition.
condTrue :: MonadZ3 m => m Cond
condTrue = mkTrue >>= return . Cond true . Just

-- | The false condition.
condFalse :: MonadZ3 m => m Cond
condFalse = mkFalse >>= return . Cond false . Just

-- | Boolean negation of a condition.
condNot :: MonadZ3 m => Cond -> m Cond
condNot = condOp1 bnot mkNot

-- | Conjunction of two conditions.
condAnd :: MonadZ3 m => Cond -> Cond -> m Cond
condAnd = condOp2 (&&&) (\l r -> mkAnd [l,r])

-- | Disjunction of two conditions.
condOr :: MonadZ3 m => Cond -> Cond -> m Cond
condOr = condOp2 (|||) (\l r -> mkOr [l,r])

-- | Apply a unary operator to a condition.
condOp1
  :: MonadZ3 m
  => (BExpr -> BExpr)  -- ^ operation on boolean expression
  -> (AST -> m AST)    -- ^ operation on symbolic encoding
  -> Cond              -- ^ condition to modify
  -> m Cond
condOp1 eOp sOp (Cond e (Just s)) = do
    s' <- sOp s
    return (Cond (eOp e) (Just s'))
condOp1 eOp _ (Cond e _) = return (Cond (eOp e) Nothing)

-- | Combine two conditions with a binary operator.
condOp2
  :: MonadZ3 m
  => (BExpr -> BExpr -> BExpr)  -- ^ operation on boolean expressions
  -> (AST -> AST -> m AST)      -- ^ operation on symbolic encodings
  -> Cond                       -- ^ left condition
  -> Cond                       -- ^ right condition
  -> m Cond
condOp2 eOp sOp (Cond e1 (Just s1)) (Cond e2 (Just s2)) = do
    s' <- sOp s1 s2
    return (Cond (eOp e1 e2) (Just s'))
condOp2 eOp _ (Cond e1 _) (Cond e2 _) = return (Cond (eOp e1 e2) Nothing)


--
-- * Boolean expressions
--

-- ** Variables

-- | The set of boolean variables referenced in a boolean expression.
boolVars :: BExpr -> Set Var
boolVars (BRef v)     = Set.singleton v
boolVars (OpB _ e)    = boolVars e
boolVars (OpBB _ l r) = boolVars l `Set.union` boolVars r
boolVars _            = Set.empty

-- | The set of integer variables referenced in a boolean expression.
intVars :: BExpr -> Set Var
intVars (BLit _)     = Set.empty
intVars (BRef _)     = Set.empty
intVars (OpB _ e)    = intVars e
intVars (OpBB _ l r) = intVars l `Set.union` intVars r
intVars (OpIB _ l r) = intVars' l `Set.union` intVars' r
  where
    intVars' (ILit _)     = Set.empty
    intVars' (IRef v)     = Set.singleton v
    intVars' (OpI _ e)    = intVars' e
    intVars' (OpII _ l r) = intVars' l `Set.union` intVars' r


-- ** Evaluation

-- | Evaluate a boolean expression to a ground boolean, given environments
--   binding all of the variables.
evalBExpr :: Env Var Bool -> Env Var Int -> BExpr -> Bool
evalBExpr _  _  (BLit b)     = b
evalBExpr mb _  (BRef v)     = envLookupOrFail v mb
evalBExpr mb mi (OpB o e)    = opB_B o (evalBExpr mb mi e)
evalBExpr mb mi (OpBB o l r) = (opBB_B o `on` evalBExpr mb mi) l r
evalBExpr _  mi (OpIB o l r) = (opNN_B o `on` evalIExpr mi) l r

-- | Evaluate an integer expression to a ground integer, given environments
--   binding all of the variables.
evalIExpr :: Env Var Int -> IExpr -> Int
evalIExpr _ (ILit i)     = i
evalIExpr m (IRef v)     = envLookupOrFail v m
evalIExpr m (OpI o e)    = opN_N o (evalIExpr m e)
evalIExpr m (OpII o l r) = (opNN_N o `on` evalIExpr m) l r


-- ** Minimization

-- | Apply some basic rules to shrink the size of a boolean expression. Does
--   not attempt to shrink integer expressions within comparison operations.
shrinkBExpr :: BExpr -> BExpr
shrinkBExpr (OpB Not e) = case shrinkBExpr e of
    BLit True  -> BLit False
    BLit False -> BLit True
    OpB Not e' -> e'
    e' -> OpB Not e'
shrinkBExpr (OpBB And l r) = case (shrinkBExpr l, shrinkBExpr r) of
    (BLit False, _) -> BLit False
    (_, BLit False) -> BLit False
    (BLit True, r') -> r'
    (l', BLit True) -> l'
    (l', r') | l' == r'  -> l'
             | otherwise -> OpBB And l' r'
shrinkBExpr (OpBB Or l r) = case (shrinkBExpr l, shrinkBExpr r) of
    (BLit True, _) -> BLit True
    (_, BLit True) -> BLit True
    (BLit False, r') -> r'
    (l', BLit False) -> l'
    (l', r') | l' == r'  -> l'
             | otherwise -> OpBB Or l' r'
shrinkBExpr e = e


-- ** Conversion to symbolic values

-- | Convert a boolean expression to a symbolic boolean with fresh variables.
symBExprFresh :: MonadZ3 m => BExpr -> m AST
symBExprFresh e = symEnv (boolVars e) (intVars e) >>= \m -> symBExpr m e

-- | Evaluate a boolean expression to a symbolic boolean, given environments
--   binding all of the variables.
symBExpr :: MonadZ3 m => SymEnv -> BExpr -> m AST
symBExpr _ (BLit b)     = mkBool b
symBExpr m (BRef x)     = return (envLookupOrFail (x,SymBool) m)
symBExpr m (OpB o e)    = symBExpr m e >>= symB_B o
symBExpr m (OpBB o l r) = do
    l' <- symBExpr m l 
    r' <- symBExpr m r
    symBB_B o l' r'
symBExpr m (OpIB o l r) = do
    l' <- symIExpr m l 
    r' <- symIExpr m r
    symNN_B o l' r'

-- | Evaluate an integer expression to a ground integer, given environments
--   binding all of the variables.
symIExpr :: MonadZ3 m => SymEnv -> IExpr -> m AST
symIExpr _ (ILit i)     = mkIntNum i
symIExpr m (IRef x)     = return (envLookupOrFail (x,SymInt) m)
symIExpr m (OpI o e)    = symIExpr m e >>= symN_N o
symIExpr m (OpII o l r) = do
    l' <- symIExpr m l 
    r' <- symIExpr m r
    symNN_N o l' r'

-- | Lookup symbolic unary ASToolean operator.
symB_B :: MonadZ3 m => B_B -> AST -> m AST
symB_B Not = mkNot

-- | Lookup symbolic unary integer operator.
symN_N :: MonadZ3 m => N_N -> AST -> m AST
symN_N Neg  n = mkUnaryMinus n
symN_N Abs  n = do
    zero <- mkIntNum (0 :: Int)
    isPos <- mkGt n zero
    mkIte isPos n =<< mkUnaryMinus n
symN_N Sign n = do
    zero <- mkIntNum (0 :: Int)
    pos <- mkIntNum (1 :: Int)
    neg <- mkIntNum (-1 :: Int)
    isNeg <- mkLt n zero
    isPos <- mkGt n zero
    mkIte isPos pos =<< mkIte isNeg neg zero

-- | Lookup symbolic ASTinary ASToolean operator.
symBB_B :: MonadZ3 m => BB_B -> AST -> AST -> m AST
symBB_B And l r = mkAnd [l,r]
symBB_B Or  l r = mkOr [l,r]
symBB_B XOr l r = mkXor l r
symBB_B Imp l r = mkImplies l r
symBB_B Eqv l r = mkEq l r

-- | Lookup symbolic ASTinary integer comparison operator.
symNN_B :: MonadZ3 m => NN_B -> AST -> AST -> m AST
symNN_B LT  l r = mkLt l r
symNN_B LTE l r = mkLe l r
symNN_B Equ l r = mkEq l r
symNN_B Neq l r = mkDistinct [l,r]
symNN_B GTE l r = mkGt l r
symNN_B GT  l r = mkGe l r

-- | Lookup symbolic ASTinary integer operator.
symNN_N :: MonadZ3 m => NN_N -> AST -> AST -> m AST
symNN_N Add l r = mkAdd [l,r]
symNN_N Sub l r = mkSub [l,r]
symNN_N Mul l r = mkMul [l,r]
symNN_N Div l r = mkDiv l r
symNN_N Mod l r = mkMod l r
