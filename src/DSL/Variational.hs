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
  dimensions :: a -> Set Var


-- ** Instances

-- Trivial instances

instance Variational PVal where
  configure _ = id
  select    _ = id
  shrink      = id
  dimensions  = mempty

instance Variational PType where
  configure _ = id
  select    _ = id
  shrink      = id
  dimensions  = mempty

instance Variational Error where
  configure _ = id
  select    _ = id
  shrink      = id
  dimensions  = mempty


-- Reduction instances

instance Variational a => Variational (V a) where
  configure c (One a) = One (configure c a)
  configure c (Chc d l r)
      | sat (c &&& d) = configure c l
      | otherwise     = configure c r

  select c (One a) = One (select c a)
  select c (Chc d l r)
      | c |=>|  d = select c l
      | c |=>!| d = select c r
      | otherwise = Chc d (select c l) (select c r)

  shrink (One a) = One a
  shrink (Chc (BLit True)  l _) = l
  shrink (Chc (BLit False) _ r) = r
  shrink (Chc d l r)
      | taut  d   = l'
      | unsat d   = r'
      | otherwise = Chc (shrinkBExpr d) l' r'
    where
      l' = shrink (select d l)
      r' = shrink (select (bnot d) r)

  dimensions (One _)     = mempty
  dimensions (Chc d l r) = boolVars d <> dimensions l <> dimensions r


-- Congruence instances

instance Variational a => Variational (Maybe a) where
  configure c = fmap (configure c)
  select    c = fmap (select c)
  shrink      = fmap shrink
  dimensions  = foldMap dimensions

instance Variational a => Variational [a] where
  configure c = map (configure c)
  select    c = map (select c)
  shrink      = map shrink
  dimensions  = foldMap dimensions

instance Variational v => Variational (Env k v) where
  configure c = fmap (configure c)
  select    c = fmap (select c)
  shrink      = fmap shrink
  dimensions  = foldMap dimensions

instance Variational Fun where
  configure c (Fun ps e) = Fun ps (configure c e)
  select    c (Fun ps e) = Fun ps (select c e)
  shrink      (Fun ps e) = Fun ps (shrink e)
  dimensions  (Fun _  e) = dimensions e

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

  dimensions (P1 _ e)        = dimensions e
  dimensions (P2 _ e1 e2)    = dimensions e1 <> dimensions e2
  dimensions (P3 _ e1 e2 e3) = dimensions e1 <> dimensions e2 <> dimensions e3
  dimensions _ = mempty

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

  dimensions (Create e) = dimensions e
  dimensions (Check  f) = dimensions f
  dimensions (Modify f) = dimensions f
  dimensions Delete     = mempty

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

  dimensions (Do _ e)     = dimensions e
  dimensions (If b t e)   = dimensions b <> dimensions t <> dimensions e
  dimensions (In _ ss)    = dimensions ss
  dimensions (Let _ e ss) = dimensions e <> dimensions ss
  dimensions (Load e es)  = dimensions e <> foldMap dimensions es

instance Variational Model where
  configure c (Model ps ss) = Model ps (configure c ss)
  select    c (Model ps ss) = Model ps (select c ss)
  shrink      (Model ps ss) = Model ps (shrink ss)
  dimensions  (Model _  ss) = dimensions ss
