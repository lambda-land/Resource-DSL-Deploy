module DSL.Variational where

import Data.Set (Set)

import DSL.Condition
import DSL.Types


--
-- * Variational type class
--

-- | Captures all kinds of objects that contain choices somewhere within them.
class Variational a where
  
  -- | Fully configure a variational value by evaluating each choice condition
  --   to either true or false. Undefined boolean dimensions are assumed to be
  --   false, undefined integer dimensions are assumed to be zero.
  configure :: Env Var Bool -> Env Var Int -> a -> a
  
  -- | Partially configure a variational value by eliminating choices whose
  --   conditions reduce to a boolean value in the given environments.
  reduce :: Env Var Bool -> Env Var Int -> a -> a
  
  -- | Get all boolean variables within choice conditions.
  boolDims :: a -> Set Var

  -- | Get all integer variables within choice conditions.
  intDims :: a -> Set Var


-- ** Instances

-- Trivial instances

instance Variational PVal where
  configure _ _ = id
  reduce    _ _ = id
  boolDims _ = mempty
  intDims  _ = mempty

instance Variational PType where
  configure _ _ = id
  reduce    _ _ = id
  boolDims _ = mempty
  intDims  _ = mempty

instance Variational Error where
  configure _ _ = id
  reduce    _ _ = id
  boolDims _ = mempty
  intDims  _ = mempty


-- Reduction instances

instance Variational a => Variational (V a) where
  configure mb mi (One a) = One (configure mb mi a)
  configure mb mi (Chc d l r)
      | evalBExpr mb mi (condExpr d) = configure mb mi l
      | otherwise                    = configure mb mi r

  reduce mb mi (One a) = One (reduce mb mi a)
  reduce mb mi (Chc (Cond e s) l r) =
      case reduceBExpr mb mi e of
        BLit True  -> l'
        BLit False -> r'
        e' -> Chc (Cond e' s) l' r'
    where
      l' = reduce mb mi l
      r' = reduce mb mi r

  boolDims (One _)     = mempty
  boolDims (Chc d l r) = boolVars (condExpr d) <> boolDims l <> boolDims r

  intDims (One _)     = mempty
  intDims (Chc d l r) = intVars (condExpr d) <> intDims l <> intDims r


-- Congruence instances

instance Variational a => Variational (Maybe a) where
  configure mb mi = fmap (configure mb mi)
  reduce    mb mi = fmap (reduce mb mi)
  boolDims = foldMap boolDims
  intDims  = foldMap intDims

instance Variational a => Variational [a] where
  configure mb mi = map (configure mb mi)
  reduce    mb mi = map (reduce mb mi)
  boolDims = foldMap boolDims
  intDims  = foldMap intDims

instance Variational v => Variational (Env k v) where
  configure mb mi = fmap (configure mb mi)
  reduce    mb mi = fmap (reduce mb mi)
  boolDims = foldMap boolDims
  intDims  = foldMap intDims

instance Variational Fun where
  configure mb mi (Fun ps e) = Fun ps (configure mb mi e)
  reduce    mb mi (Fun ps e) = Fun ps (reduce mb mi e)
  boolDims (Fun _  e) = boolDims e
  intDims  (Fun _  e) = intDims e

instance Variational Expr where
  configure mb mi (P1 o e)        = P1 o (configure mb mi e)
  configure mb mi (P2 o e1 e2)    = P2 o (configure mb mi e1) (configure mb mi e2)
  configure mb mi (P3 o e1 e2 e3) = P3 o (configure mb mi e1) (configure mb mi e2) (configure mb mi e3)
  configure _ _ e = e
  
  reduce mb mi (P1 o e)        = P1 o (reduce mb mi e)
  reduce mb mi (P2 o e1 e2)    = P2 o (reduce mb mi e1) (reduce mb mi e2)
  reduce mb mi (P3 o e1 e2 e3) = P3 o (reduce mb mi e1) (reduce mb mi e2) (reduce mb mi e3)
  reduce _ _ e = e

  boolDims (P1 _ e)        = boolDims e
  boolDims (P2 _ e1 e2)    = boolDims e1 <> boolDims e2
  boolDims (P3 _ e1 e2 e3) = boolDims e1 <> boolDims e2 <> boolDims e3
  boolDims _ = mempty

  intDims (P1 _ e)        = intDims e
  intDims (P2 _ e1 e2)    = intDims e1 <> intDims e2
  intDims (P3 _ e1 e2 e3) = intDims e1 <> intDims e2 <> intDims e3
  intDims _ = mempty

instance Variational Effect where
  configure mb mi (Create e) = Create (configure mb mi e)
  configure mb mi (Check  f) = Check  (configure mb mi f)
  configure mb mi (Modify f) = Modify (configure mb mi f)
  configure _ _ Delete     = Delete
  
  reduce mb mi (Create e) = Create (reduce mb mi e)
  reduce mb mi (Check  f) = Check  (reduce mb mi f)
  reduce mb mi (Modify f) = Modify (reduce mb mi f)
  reduce _ _ Delete     = Delete

  boolDims (Create e) = boolDims e
  boolDims (Check  f) = boolDims f
  boolDims (Modify f) = boolDims f
  boolDims Delete     = mempty

  intDims (Create e) = intDims e
  intDims (Check  f) = intDims f
  intDims (Modify f) = intDims f
  intDims Delete     = mempty

instance Variational Stmt where
  configure mb mi (Do p e)     = Do p (configure mb mi e)
  configure mb mi (If b t e)   = If (configure mb mi b) (configure mb mi t) (configure mb mi e)
  configure mb mi (In p ss)    = In p (configure mb mi ss)
  configure mb mi (Let v e ss) = Let v (configure mb mi e) (configure mb mi ss)
  configure mb mi (Load e es)  = Load (configure mb mi e) (map (configure mb mi) es)
  
  reduce mb mi (Do p e)     = Do p (reduce mb mi e)
  reduce mb mi (If b t e)   = If (reduce mb mi b) (reduce mb mi t) (reduce mb mi e)
  reduce mb mi (In p ss)    = In p (reduce mb mi ss)
  reduce mb mi (Let v e ss) = Let v (reduce mb mi e) (reduce mb mi ss)
  reduce mb mi (Load e es)  = Load (reduce mb mi e) (map (reduce mb mi) es)

  boolDims (Do _ e)     = boolDims e
  boolDims (If b t e)   = boolDims b <> boolDims t <> boolDims e
  boolDims (In _ ss)    = boolDims ss
  boolDims (Let _ e ss) = boolDims e <> boolDims ss
  boolDims (Load e es)  = boolDims e <> boolDims es

  intDims (Do _ e)     = intDims e
  intDims (If b t e)   = intDims b <> intDims t <> intDims e
  intDims (In _ ss)    = intDims ss
  intDims (Let _ e ss) = intDims e <> intDims ss
  intDims (Load e es)  = intDims e <> intDims es

instance Variational Model where
  configure mb mi (Model ps ss) = Model ps (configure mb mi ss)
  reduce    mb mi (Model ps ss) = Model ps (reduce mb mi ss)
  boolDims (Model _  ss) = boolDims ss
  intDims  (Model _  ss) = intDims ss
