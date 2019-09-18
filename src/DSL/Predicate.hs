module DSL.Predicate where

import Prelude hiding (LT,GT)

import Data.Function (on)
import Data.SBV.Trans (MonadSymbolic,Symbolic,SBool,SInt32,sBool,sInt32)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (unpack)

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


-- ** Evaluation to plain and symbolic values

-- | Construct an environment with fresh symbolic values for each variable.
symEnv :: MonadSymbolic m => (Name -> m b) -> Set Name -> m (Env Var b)
symEnv f s = fmap (envFromList . zip vs) (mapM f vs)
  where vs = Set.toList s

-- | Evaluate a boolean expression to either a ground or symbolic boolean,
--   given a corresponding dictionary of comparison operators and
--   environments binding all of the variables.
evalBExpr :: Prim b i => Env Var b -> Env Var i -> BExpr -> b
evalBExpr _  _  (BLit b)     = fromBool b
evalBExpr mb _  (BRef v)     = envLookupOrFail v mb
evalBExpr mb mi (OpB o e)    = opB_B o (evalBExpr mb mi e)
evalBExpr mb mi (OpBB o l r) = (opBB_B o `on` evalBExpr mb mi) l r
evalBExpr _  mi (OpIB o l r) = (opNN_B o `on` evalIExpr mi) l r

-- | Evaluate an integer expression to either a ground or symbolic integer,
--   given an environment binding all of the variables.
evalIExpr :: PrimN i => Env Var i -> IExpr -> i
evalIExpr _ (ILit i)     = fromIntegral i
evalIExpr m (IRef v)     = envLookupOrFail v m
evalIExpr m (OpI o e)    = opN_N o (evalIExpr m e)
evalIExpr m (OpII o l r) = (opNN_N o `on` evalIExpr m) l r

-- | Evaluate a boolean expression to a ground boolean.
toBool :: Env Var Bool -> Env Var Int -> BExpr -> Bool
toBool = evalBExpr

-- | Evaluate a boolean expression to a symbolic boolean.
toSBool :: Env Var SBool -> Env Var SInt32 -> BExpr -> SBool
toSBool = evalBExpr

-- | Convert a boolean expression to a symbolic boolean with fresh variables.
toSBoolFresh :: BExpr -> Symbolic SBool
toSBoolFresh e = do
    mb <- symEnv (sBool . unpack) (boolVars e)
    mi <- symEnv (sInt32 . unpack) (intVars e)
    return (toSBool mb mi e)


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
