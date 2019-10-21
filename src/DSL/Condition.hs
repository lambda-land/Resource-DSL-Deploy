module DSL.Condition where

import Prelude hiding (LT,GT)

import Control.Monad.IO.Class (liftIO)
import Data.Function (on)
import Data.Maybe (fromMaybe)
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

-- | Get the symbolic represenation of a condition or fail if it's not there.
condSymOrFail :: Cond -> AST
condSymOrFail (Cond _ (Just s)) = s
condSymOrFail c = error $ "condSymOrFail: no symbolic representation of: " ++ show c

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
    let e' = shrinkBExpr (eOp e)
    s' <- sOp s
    return (Cond e' (Just s'))
condOp1 _ _ c = errorUnprepped c

-- | Combine two conditions with a binary operator.
condOp2
  :: MonadZ3 m
  => (BExpr -> BExpr -> BExpr)  -- ^ operation on boolean expressions
  -> (AST -> AST -> m AST)      -- ^ operation on symbolic encodings
  -> Cond                       -- ^ left condition
  -> Cond                       -- ^ right condition
  -> m Cond
condOp2 eOp sOp (Cond e1 (Just s1)) (Cond e2 (Just s2)) = do
    let e' = shrinkBExpr (eOp e1 e2)
    s' <- sOp s1 s2
    return (Cond e' (Just s'))
condOp2 _ _ c@(Cond _ Nothing) _ = errorUnprepped c
condOp2 _ _ _ c@(Cond _ Nothing) = errorUnprepped c


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
--   binding its variables. Unbound boolean variables are assumed to be false,
--   unbound integer variables are zero.
evalBExpr :: Env Var Bool -> Env Var Int -> BExpr -> Bool
evalBExpr _  _  (BLit b)     = b
evalBExpr mb _  (BRef x)     = fromMaybe False (envLookup x mb)
evalBExpr mb mi (OpB o e)    = opB_B o (evalBExpr mb mi e)
evalBExpr mb mi (OpBB o l r) = (opBB_B o `on` evalBExpr mb mi) l r
evalBExpr _  mi (OpIB o l r) = (opNN_B o `on` evalIExpr mi) l r

-- | Evaluate an integer expression to a ground integer, given environments
--   binding its variables. Unbound variables are assumed to be zero.
evalIExpr :: Env Var Int -> IExpr -> Int
evalIExpr _ (ILit i)     = i
evalIExpr m (IRef x)     = fromMaybe 0 (envLookup x m)
evalIExpr m (OpI o e)    = opN_N o (evalIExpr m e)
evalIExpr m (OpII o l r) = (opNN_N o `on` evalIExpr m) l r

-- | Partially evaluate a boolean expression given definitions for some of
--   its variables.
reduceBExpr :: Env Var Bool -> Env Var Int -> BExpr -> BExpr
reduceBExpr _  _  e@(BLit _) = e
reduceBExpr mb _  e@(BRef x) = maybe e BLit (envLookup x mb)
reduceBExpr mb mi (OpB o e) =
    case reduceBExpr mb mi e of
      BLit b -> BLit (opB_B o b)
      e'     -> OpB o e'
reduceBExpr mb mi (OpBB o l r) =
    case (reduceBExpr mb mi l, reduceBExpr mb mi r) of
      (BLit lb, BLit rb) -> BLit (opBB_B o lb rb)
      (l', r')           -> OpBB o l' r'
reduceBExpr _  mi (OpIB o l r) =
    case (reduceIExpr mi l, reduceIExpr mi r) of
      (ILit li, ILit ri) -> BLit (opNN_B o li ri)
      (l', r')           -> OpIB o l' r'

-- | Partially evaluate an integer expression given definitions for some of
--   its variables.
reduceIExpr :: Env Var Int -> IExpr -> IExpr
reduceIExpr _ e@(ILit _) = e
reduceIExpr m e@(IRef x) = maybe e ILit (envLookup x m)
reduceIExpr m (OpI o e) =
    case reduceIExpr m e of
      ILit i -> ILit (opN_N o i)
      e'     -> OpI o e'
reduceIExpr m (OpII o l r) =
    case (reduceIExpr m l, reduceIExpr m r) of
      (ILit li, ILit ri) -> ILit (opNN_N o li ri)
      (l', r')           -> OpII o l' r'


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
symBExprFresh e = do
    z3 <- getSolver
    ctx <- getContext
    syms <- liftIO $ symEnvFresh (z3,ctx) (boolVars e) (intVars e)
    symBExpr syms e

-- | Evaluate a boolean expression to a symbolic boolean, given environments
--   binding all of the variables.
symBExpr :: MonadZ3 m => SymEnv -> BExpr -> m AST
symBExpr _ (BLit b)     = mkBool b
symBExpr m (BRef x)     = return (envLookupOrFail "symBExpr" (x,OptBool) m)
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
symIExpr m (IRef x)     = return (envLookupOrFail "symIExpr" (x,OptInt) m)
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
