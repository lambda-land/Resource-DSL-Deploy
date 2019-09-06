module DSL.V where

import Data.Set (Set)

import DSL.Boolean
import DSL.Name
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
  shrink (Chc d l r)
      | taut  d   = shrink l
      | unsat d   = shrink r
      | otherwise = Chc (shrinkBExpr d) (shrink l) (shrink r)

  dimensions (One _)     = mempty
  dimensions (Chc d l r) = boolVars d <> dimensions l <> dimensions r


instance Variational a => Variational (SegList a) where
  configure c = concatMap segment
    where
      segment (Elems xs) = [Elems (map (configure c) xs)]
      segment (Split d l r)
        | sat (c &&& d) = configure c l
        | otherwise     = configure c r
        
  select c = concatMap segment
    where
      segment (Elems xs) = [Elems (map (select c) xs)]
      segment (Split d l r)
        | c |=>|  d = select c l
        | c |=>!| d = select c r
        | otherwise = [Split d (select c l) (select c r)]
  
  shrink = concatMap segment
    where
      segment (Elems xs) = [Elems (map shrink xs)]
      segment (Split d l r)
          | taut  d   = shrink l
          | unsat d   = shrink r
          | otherwise = [Split (shrinkBExpr d) (shrink l) (shrink r)]

  dimensions [] = mempty
  dimensions (Elems xs : ys) = foldMap dimensions xs <> dimensions ys
  dimensions (Split d l r : ys) = boolVars d <> dimensions l <> dimensions r <> dimensions ys


-- Congruence instances

instance Variational Param where
  configure c (Param x t) = Param x (configure c t)
  select    c (Param x t) = Param x (select c t)
  shrink      (Param x t) = Param x (shrink t)
  dimensions  (Param _ t) = dimensions t

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

instance Variational a => Variational [V a] where
  configure c = map (configure c)
  select    c = map (select c)
  shrink      = map shrink
  dimensions  = foldMap dimensions

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
  configure c (For v e ss) = For v (configure c e) (configure c ss)
  configure c (Let v e ss) = Let v (configure c e) (configure c ss)
  configure c (Load e es)  = Load (configure c e) (map (configure c) es)
  
  select c (Do p e)     = Do p (select c e)
  select c (If b t e)   = If (select c b) (select c t) (select c e)
  select c (In p ss)    = In p (select c ss)
  select c (For v e ss) = For v (select c e) (select c ss)
  select c (Let v e ss) = Let v (select c e) (select c ss)
  select c (Load e es)  = Load (select c e) (map (select c) es)

  shrink (Do p e)     = Do p (shrink e)
  shrink (If b t e)   = If (shrink b) (shrink t) (shrink e)
  shrink (In p ss)    = In p (shrink ss)
  shrink (For v e ss) = For v (shrink e) (shrink ss)
  shrink (Let v e ss) = Let v (shrink e) (shrink ss)
  shrink (Load e es)  = Load (shrink e) (map (shrink) es)

  dimensions (Do _ e)     = dimensions e
  dimensions (If b t e)   = dimensions b <> dimensions t <> dimensions e
  dimensions (In _ ss)    = dimensions ss
  dimensions (For _ e ss) = dimensions e <> dimensions ss
  dimensions (Let _ e ss) = dimensions e <> dimensions ss
  dimensions (Load e es)  = dimensions e <> foldMap dimensions es

instance Variational Model where
  configure c (Model ps ss) = Model (map (configure c) ps) (configure c ss)
  select    c (Model ps ss) = Model (map (select c) ps) (select c ss)
  shrink      (Model ps ss) = Model (map shrink ps) (shrink ss)
  dimensions  (Model ps ss) = foldMap dimensions ps <> dimensions ss

instance Variational Profile where
  configure c (Profile ps es) = Profile (map (configure c) ps) (fmap (configure c) es)
  select    c (Profile ps es) = Profile (map (select c) ps) (fmap (select c) es)
  shrink      (Profile ps es) = Profile (map shrink ps) (fmap shrink es)
  dimensions  (Profile ps es) = foldMap dimensions ps <> foldMap dimensions es

instance Variational Entry where
  configure c (ProEntry p) = ProEntry (configure c p)
  configure c (ModEntry m) = ModEntry (configure c m)

  select c (ProEntry p) = ProEntry (select c p)
  select c (ModEntry m) = ModEntry (select c m)

  shrink (ProEntry p) = ProEntry (shrink p)
  shrink (ModEntry m) = ModEntry (shrink m)
  
  dimensions (ProEntry p) = dimensions p
  dimensions (ModEntry m) = dimensions m

instance Variational StateCtx where
  configure c (SCtx r e m) = SCtx (configure c r) e (configure c m)
  select    c (SCtx r e m) = SCtx (select c r) e (select c m)
  shrink      (SCtx r e m) = SCtx (shrink r) e (shrink m)
  dimensions  (SCtx r _ m) = dimensions r <> dimensions m

instance Variational v => Variational (Env k v) where
  configure c = fmap (configure c)
  select    c = fmap (select c)
  shrink      = fmap shrink
  dimensions  = foldMap dimensions

instance Variational a => Variational (Maybe a) where
  configure c = fmap (configure c)
  select    c = fmap (select c)
  shrink      = fmap shrink
  dimensions  = foldMap dimensions


--
-- * Other functions
--

-- | Merge second variational error into the first.
mergeVError :: VError -> VError -> VError
-- If there's no error in one of the masks, then just choose the other mask
mergeVError (One Nothing) m = m
mergeVError m (One Nothing) = m
-- An error in the original mask dominates the mergee
mergeVError e@(One (Just _)) _ = e
-- Merge a single error with the leaves of the choice tree
mergeVError (Chc d l r ) e@(One (Just _)) = Chc d (mergeVError l e) (mergeVError r e)
-- Merge errors in variational contexts
mergeVError m@(Chc d l r) m'@(Chc d' l' r')
  -- If the dimensions are equiv, merge both sides with each other
  | d |=|   d'  = Chc d (mergeVError l l') (mergeVError r r')
  -- If d |<=| d', merge new mask only into left choice
  | d |<=|  d' = Chc d (mergeVError l m') r
  -- If d |!<=| d', merge new mask only into right choice
  | d |!<=| d' = Chc d l (mergeVError r m')
  -- If d |=>| d', merge original mask into left choice of new mask
  | d |=>|  d' = Chc d' (mergeVError m l') r'
  -- If d |=>!| d', merge original mask into right choice of new mask
  | d |=>!| d' = Chc d' l' (mergeVError m r')
  -- Otherwise, merge new mask into both sides of choice
  | otherwise  = Chc d (mergeVError l m') (mergeVError r m')

-- | Turns a variational value into a variational optional value
toVMaybe :: V a -> V (Maybe a)
toVMaybe = fmap Just
