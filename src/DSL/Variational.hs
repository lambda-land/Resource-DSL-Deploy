module DSL.Variational where

import Data.Set (Set)

import DSL.Boolean
import DSL.Predicate
import DSL.SAT
import DSL.Types


--
-- * Variational type class
--

-- | A type class capturing all kinds of terms that may contain variation.
class Variational a where
  
  -- | Fully configure a term according to the given partial configuration.
  --   The first alternative is chosen if it is consistent with the partial
  --   configuration, otherwise the second is chosen.
  configure :: BExpr -> a -> a

  -- | Eliminate all choices in a term that are determined by a partial
  --   configuration expressed as a boolean expression.
  select :: BExpr -> a -> a

  -- | Shrink the size of a term by applying some basic equational laws.
  shrink :: a -> a

  -- | Get all boolean variables within choice conditions.
  boolDims :: a -> Set Var

  -- | Get all integer variables within choice conditions.
  intDims :: a -> Set Var


-- ** Instances

-- Trivial instances

instance Variational PVal where
  configure _ = id
  select    _ = id
  shrink      = id
  boolDims    = mempty
  intDims     = mempty

instance Variational PType where
  configure _ = id
  select    _ = id
  shrink      = id
  boolDims    = mempty
  intDims     = mempty

instance Variational Error where
  configure _ = id
  select    _ = id
  shrink      = id
  boolDims    = mempty
  intDims     = mempty


-- Reduction instances

instance Variational a => Variational (V a) where
  configure c (One a) = One (configure c a)
  configure c (Chc d l r)
      | sat (c &&& d) = configure c l
      | otherwise     = configure c r

  select c (One a) = One (select c a)
  select c (Chc d l r)
      | implies  c d = select c l
      | nimplies c d = select c r
      | otherwise    = Chc d (select c l) (select c r)

  shrink (One a) = One a
  shrink (Chc (BLit True)  l _) = l
  shrink (Chc (BLit False) _ r) = r
  shrink (Chc (OpB Not d) l r)  = shrink (Chc d r l)
  shrink (Chc d l r)
      | taut  d   = l'
      | unsat d   = r'
      | otherwise = Chc (shrinkBExpr d) l' r'
    where
      l' = shrink (select d l)
      r' = shrink (select (bnot d) r)

  boolDims (One _)     = mempty
  boolDims (Chc d l r) = boolVars d <> boolDims l <> boolDims r

  intDims (One _)     = mempty
  intDims (Chc d l r) = intVars d <> intDims l <> intDims r


-- Congruence instances

instance Variational a => Variational (Maybe a) where
  configure c = fmap (configure c)
  select    c = fmap (select c)
  shrink      = fmap shrink
  boolDims    = foldMap boolDims
  intDims     = foldMap intDims

instance Variational a => Variational [a] where
  configure c = map (configure c)
  select    c = map (select c)
  shrink      = map shrink
  boolDims    = foldMap boolDims
  intDims     = foldMap intDims

instance Variational v => Variational (Env k v) where
  configure c = fmap (configure c)
  select    c = fmap (select c)
  shrink      = fmap shrink
  boolDims    = foldMap boolDims
  intDims     = foldMap intDims

instance Variational Fun where
  configure c (Fun ps e) = Fun ps (configure c e)
  select    c (Fun ps e) = Fun ps (select c e)
  shrink      (Fun ps e) = Fun ps (shrink e)
  boolDims    (Fun _  e) = boolDims e
  intDims     (Fun _  e) = intDims e

instance Variational Expr where
  configure c (P1 o e)        = P1 o (configure c e)
  configure c (P2 o e1 e2)    = P2 o (configure c e1) (configure c e2)
  configure c (P3 o e1 e2 e3) = P3 o (configure c e1) (configure c e2) (configure c e3)
  configure _ e = e
  
  select c (P1 o e)        = P1 o (select c e)
  select c (P2 o e1 e2)    = P2 o (select c e1) (select c e2)
  select c (P3 o e1 e2 e3) = P3 o (select c e1) (select c e2) (select c e3)
  select _ e = e

  shrink (P1 o e)        = P1 o (shrink e)
  shrink (P2 o e1 e2)    = P2 o (shrink e1) (shrink e2)
  shrink (P3 o e1 e2 e3) = P3 o (shrink e1) (shrink e2) (shrink e3)
  shrink e = e

  boolDims (P1 _ e)        = boolDims e
  boolDims (P2 _ e1 e2)    = boolDims e1 <> boolDims e2
  boolDims (P3 _ e1 e2 e3) = boolDims e1 <> boolDims e2 <> boolDims e3
  boolDims _ = mempty

  intDims (P1 _ e)        = intDims e
  intDims (P2 _ e1 e2)    = intDims e1 <> intDims e2
  intDims (P3 _ e1 e2 e3) = intDims e1 <> intDims e2 <> intDims e3
  intDims _ = mempty

instance Variational Effect where
  configure c (Create e) = Create (configure c e)
  configure c (Check  f) = Check  (configure c f)
  configure c (Modify f) = Modify (configure c f)
  configure _ Delete     = Delete
  
  select c (Create e) = Create (select c e)
  select c (Check  f) = Check  (select c f)
  select c (Modify f) = Modify (select c f)
  select _ Delete     = Delete

  shrink (Create e) = Create (shrink e)
  shrink (Check  f) = Check  (shrink f)
  shrink (Modify f) = Modify (shrink f)
  shrink Delete     = Delete

  boolDims (Create e) = boolDims e
  boolDims (Check  f) = boolDims f
  boolDims (Modify f) = boolDims f
  boolDims Delete     = mempty

  intDims (Create e) = intDims e
  intDims (Check  f) = intDims f
  intDims (Modify f) = intDims f
  intDims Delete     = mempty

instance Variational Stmt where
  configure c (Do p e)     = Do p (configure c e)
  configure c (If b t e)   = If (configure c b) (configure c t) (configure c e)
  configure c (In p ss)    = In p (configure c ss)
  configure c (Let v e ss) = Let v (configure c e) (configure c ss)
  configure c (Load e es)  = Load (configure c e) (map (configure c) es)
  
  select c (Do p e)     = Do p (select c e)
  select c (If b t e)   = If (select c b) (select c t) (select c e)
  select c (In p ss)    = In p (select c ss)
  select c (Let v e ss) = Let v (select c e) (select c ss)
  select c (Load e es)  = Load (select c e) (map (select c) es)

  shrink (Do p e)     = Do p (shrink e)
  shrink (If b t e)   = If (shrink b) (shrink t) (shrink e)
  shrink (In p ss)    = In p (shrink ss)
  shrink (Let v e ss) = Let v (shrink e) (shrink ss)
  shrink (Load e es)  = Load (shrink e) (map (shrink) es)

  boolDims (Do _ e)     = boolDims e
  boolDims (If b t e)   = boolDims b <> boolDims t <> boolDims e
  boolDims (In _ ss)    = boolDims ss
  boolDims (Let _ e ss) = boolDims e <> boolDims ss
  boolDims (Load e es)  = boolDims e <> foldMap boolDims es

  intDims (Do _ e)     = intDims e
  intDims (If b t e)   = intDims b <> intDims t <> intDims e
  intDims (In _ ss)    = intDims ss
  intDims (Let _ e ss) = intDims e <> intDims ss
  intDims (Load e es)  = intDims e <> foldMap intDims es

instance Variational Model where
  configure c (Model ps ss) = Model ps (configure c ss)
  select    c (Model ps ss) = Model ps (select c ss)
  shrink      (Model ps ss) = Model ps (shrink ss)
  boolDims    (Model _  ss) = boolDims ss
  intDims     (Model _  ss) = intDims ss

instance Variational StateCtx where
  configure c (SCtx r a e m) = SCtx (configure c r) a e (configure c m)
  select    c (SCtx r a e m) = SCtx (select c r) a e (select c m)
  shrink      (SCtx r a e m) = SCtx (shrink r) a e (shrink m)
  boolDims    (SCtx r _ _ m) = boolDims r <> boolDims m
  intDims     (SCtx r _ _ m) = intDims r <> intDims m
