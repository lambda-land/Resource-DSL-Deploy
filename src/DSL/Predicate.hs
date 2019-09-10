module DSL.Predicate where

import Prelude hiding (LT,GT)

import Data.Function (on)
import Data.Set (Set)
import qualified Data.Set as Set

import Data.SBV (SBool,SInt32,Symbolic)

import DSL.Boolean
import DSL.Types
import DSL.Environment
import DSL.Primitive

--
-- * Predicate Expressions
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

-- | Rename a variable in a expression.
renameVar :: Var -> Var -> BExpr -> BExpr
renameVar _   _   p@(BLit _)   = p
renameVar old new p@(BRef v)   = if v == old then BRef new else p
renameVar old new (OpB o e)    = OpB o (renameVar old new e)
renameVar old new (OpBB o l r) = OpBB o (renameVar old new l) (renameVar old new r)
renameVar old new (OpIB o l r) = OpIB o (renameVar' l) (renameVar' r)
  where
    renameVar' p@(ILit _)   = p
    renameVar' p@(IRef v)   = if v == old then IRef new else p
    renameVar' (OpI o e)    = OpI o (renameVar' e)
    renameVar' (OpII o l r) = OpII o (renameVar' l) (renameVar' r)


-- ** Substitution

-- | Substitute a boolean variable in a boolean expression.
substB :: Var -> Bool -> BExpr -> BExpr
substB v b e@(BRef w)   = if w == v then BLit b else e
substB v b (OpB o e)    = OpB o (substB v b e)
substB v b (OpBB o l r) = OpBB o (substB v b l) (substB v b r)
substB _ _ e            = e

-- | Substitute an integer variable in a boolean expression.
substI :: Var -> Int -> BExpr -> BExpr
substI _ _ e@(BLit _)   = e
substI _ _ e@(BRef _)   = e
substI v i (OpB o e)    = OpB o (substI v i e)
substI v i (OpBB o l r) = OpBB o (substI v i l) (substI v i r)
substI v i (OpIB o l r) = OpIB o (substI' l) (substI' r)
  where
    substI' e@(ILit _)   = e
    substI' e@(IRef w)   = if w == v then ILit i else e
    substI' (OpI o e)    = OpI o (substI' e)
    substI' (OpII o l r) = OpII o (substI' l) (substI' r)


-- ** Evaluation to plain and symbolic values

-- | Construct an environment with fresh symbolic values for each variable.
symEnv :: (Name -> Symbolic b) -> Set Name -> Symbolic (Env Var b)
symEnv f s = fmap (envFromList . zip vs) (mapM f vs)
  where vs = Set.toList s

-- | Evaluate a predicate against a primitive value.
evalPred :: Env Var Bool -> Env Var Int -> Pred -> PVal -> Bool
evalPred _  _  UPred       Unit  = true
evalPred mb mi (BPred x e) (B b) = evalBExpr (envExtend x b mb) mi e
evalPred mb mi (IPred x e) (I i) = evalBExpr mb (envExtend x i mi) e
evalPred _ _ p v = error $ unlines
    [ "evalPred: type error"
    , "  predicate: " ++ show p
    , "  value: " ++ show v ]

-- | Evaluate a boolean expression to either a ground or symbolic boolean,
--   given a corresponding dictionary of comparison operators and
--   environments binding all of the variables.
evalBExpr :: Prim b i => Env Var b -> Env Var i -> BExpr -> b
evalBExpr _  _  (BLit b)     = fromBool b
evalBExpr mb _  (BRef v)     = assumeSuccess (envLookup v mb)
evalBExpr mb mi (OpB o e)    = opB_B o (evalBExpr mb mi e)
evalBExpr mb mi (OpBB o l r) = (opBB_B o `on` evalBExpr mb mi) l r
evalBExpr _  mi (OpIB o l r) = (opNN_B o `on` evalIExpr mi) l r

-- | Evaluate an integer expression to either a ground or symbolic integer,
--   given an environment binding all of the variables.
evalIExpr :: PrimN i => Env Var i -> IExpr -> i
evalIExpr _ (ILit i)     = fromIntegral i
evalIExpr m (IRef v)     = assumeSuccess (envLookup v m)
evalIExpr m (OpI o e)    = opN_N o (evalIExpr m e)
evalIExpr m (OpII o l r) = (opNN_N o `on` evalIExpr m) l r

-- | Evaluate a boolean expression to a ground boolean.
toBool :: Env Var Bool -> Env Var Int -> BExpr -> Bool
toBool = evalBExpr

-- | Evaluate a boolean expression to a symbolic boolean.
toSBool :: Env Var SBool -> Env Var SInt32 -> BExpr -> SBool
toSBool = evalBExpr


-- ** Minimization

-- | Apply some basic rules to shrink the size of a boolean expression. Does
--   not attempt to shrink integer expressions within comparison operations.
shrinkBExpr :: BExpr -> BExpr
shrinkBExpr (OpB Not e) = case shrinkBExpr e of
    BLit True  -> BLit False
    BLit False -> BLit True
    e' -> OpB Not e'
shrinkBExpr (OpBB And l r) = case (shrinkBExpr l, shrinkBExpr r) of
    (BLit False, _) -> BLit False
    (_, BLit False) -> BLit False
    (BLit True, r') -> r'
    (l', BLit True) -> l'
    (l', r') -> OpBB And l' r'
shrinkBExpr (OpBB Or l r) = case (shrinkBExpr l, shrinkBExpr r) of
    (BLit True, _) -> BLit True
    (_, BLit True) -> BLit True
    (BLit False, r') -> r'
    (l', BLit False) -> l'
    (l', r') -> OpBB Or l' r'
shrinkBExpr e = e
