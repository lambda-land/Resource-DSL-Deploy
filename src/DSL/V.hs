module DSL.V where

import Data.SBV (Boolean(..))
import Data.Set (Set)
import qualified Data.Set as S

import DSL.Types
import DSL.Name
import DSL.Predicate
import DSL.SAT


--
-- * Variational type class
--

-- | A type class capturing all kinds of terms that may contain variation.
class Variational a where
  
  -- | Eliminate all choices in a term that are determined by a partial
  --   configuration expressed as a boolean expression.
  select :: BExpr -> a -> a

  -- | Fully configure a term according to the given partial configuration.
  --   The first alternative is chosen if it is consistent with the partial
  --   configuration, otherwise the second is chosen.
  configure :: BExpr -> a -> a

  -- | Get all boolean variables within choice conditions.
  dimensions :: a -> Set Var


-- ** Instances

instance Variational a => Variational (V a) where
  select c (One a) = One (select c a)
  select c (Chc d l r)
    | c |=>|  d = select c l
    | c |=>!| d = select c r
    | otherwise = Chc d (select c l) (select c r)

  configure c (One a) = One (configure c a)
  configure c (Chc d l r)
    | sat (c &&& d) = configure c l
    | otherwise     = configure c r

  dimensions (One _)     = mempty
  dimensions (Chc d l r) = boolVars d <> dimensions l <> dimensions r

instance Variational PVal where
  select _ = id
  configure _ = id
  dimensions = mempty

instance Variational PType where
  select _ = id
  configure _ = id
  dimensions = mempty

instance Variational Param where
  select c (Param v t) = Param v (select c t)
  configure c (Param v t) = Param v (configure c t)
  dimensions (Param _ t) = dimensions t

instance Variational Fun where
  select c (Fun p e) = Fun (select c p) (select c e)
  configure c (Fun p e) = Fun (configure c p) (configure c e)
  dimensions (Fun p e) = dimensions p <> dimensions e

instance Variational Expr where
  select c (Lit pv) = Lit (select c pv)
  select c (P1 o e) = P1 o (select c e)
  select c (P2 o e1 e2) = P2 o (select c e1) (select c e2)
  select c (P3 o e1 e2 e3) = P3 o (select c e1) (select c e2) (select c e3)
  select _ e = e

  configure c (Lit pv) = Lit (configure c pv)
  configure c (P1 o e) = P1 o (configure c e)
  configure c (P2 o e1 e2) = P2 o (configure c e1) (configure c e2)
  configure c (P3 o e1 e2 e3) = P3 o (configure c e1) (configure c e2) (configure c e3)
  configure _ e = e

  dimensions (Lit pv) = dimensions pv
  dimensions (P1 _ e) = dimensions e
  dimensions (P2 _ e1 e2) = dimensions e1 <> dimensions e2
  dimensions (P3 _ e1 e2 e3) = dimensions e1 <> dimensions e2 <> dimensions e3
  dimensions _ = mempty

instance Variational [V PVal] where
  select c xs = map (select c) xs
  configure c xs = map (configure c) xs
  dimensions xs = foldMap dimensions xs

instance Variational a => Variational (SegList a) where
  select _ [] = []
  select c (Elems xs : ys) = Elems (map (select c) xs) : select c ys
  select c (Split d l r : ys)
    | c |=>| d  = select c l ++ select c ys
    | c |=>!| d = select c r ++ select c ys
    | otherwise = Split d (select c l) (select c r) : select c ys

  configure _ [] = []
  configure c (Elems xs : ys) = Elems (map (configure c) xs) : configure c ys
  configure c (Split d l r : ys)
    | sat (c &&& d) = configure c l ++ configure c ys
    | otherwise     = configure c r ++ configure c ys

  dimensions [] = mempty
  dimensions (Elems xs : ys) = foldMap dimensions xs <> dimensions ys
  dimensions (Split d l r : ys) = boolVars d <> dimensions l <> dimensions r <> dimensions ys

instance Variational Effect where
  select c (Create e) = Create (select c e)
  select c (Check f)  = Check (select c f)
  select c (Modify f) = Modify (select c f)
  select _ Delete     = Delete

  configure c (Create e) = Create (configure c e)
  configure c (Check f)  = Check (configure c f)
  configure c (Modify f) = Modify (configure c f)
  configure _ Delete     = Delete

  dimensions (Create e) = dimensions e
  dimensions (Check f)  = dimensions f
  dimensions (Modify f) = dimensions f
  dimensions Delete     = mempty

instance Variational Stmt where
  select c (Do p e)     = Do p (select c e)
  select c (If b t e)   = If (select c b) (select c t) (select c e)
  select c (In p ss)    = In p (select c ss)
  select c (For v e ss) = For v (select c e) (select c ss)
  select c (Let v e ss) = Let v (select c e) (select c ss)
  select c (Load e es)  = Load (select c e) (map (select c) es)

  configure c (Do p e)     = Do p (configure c e)
  configure c (If b t e)   = If (configure c b) (configure c t) (configure c e)
  configure c (In p ss)    = In p (configure c ss)
  configure c (For v e ss) = For v (configure c e) (configure c ss)
  configure c (Let v e ss) = Let v (configure c e) (configure c ss)
  configure c (Load e es)  = Load (configure c e) (map (configure c) es)

  dimensions (Do _ e)     = dimensions e
  dimensions (If b t e)   = dimensions b <> dimensions t <> dimensions e
  dimensions (In _ ss)    = dimensions ss
  dimensions (For _ e ss) = dimensions e <> dimensions ss
  dimensions (Let _ e ss) = dimensions e <> dimensions ss
  dimensions (Load e es)  = dimensions e <> foldMap dimensions es

instance Variational Model where
  select c (Model ps ss) = Model (map (select c) ps) (select c ss)
  configure c (Model ps ss) = Model (map (configure c) ps) (configure c ss)
  dimensions (Model ps ss) = foldMap dimensions ps <> dimensions ss

instance Variational Profile where
  select c (Profile ps es) = Profile (map (select c) ps) (fmap (select c) es)
  configure c (Profile ps es) = Profile (map (configure c) ps) (fmap (configure c) es)
  dimensions (Profile ps es) = foldMap dimensions ps <> foldMap dimensions es

instance Variational Entry where
  select c (ProEntry p) = ProEntry (select c p)
  select c (ModEntry m) = ModEntry (select c m)

  configure c (ProEntry p) = ProEntry (configure c p)
  configure c (ModEntry m) = ModEntry (configure c m)

  dimensions (ProEntry p) = dimensions p
  dimensions (ModEntry m) = dimensions m

instance Variational v => Variational (Env k v) where
  select c env = fmap (select c) env
  configure c env = fmap (configure c) env
  dimensions = foldMap dimensions

instance Variational a => Variational (Maybe a) where
  select c m = fmap (select c) m
  configure c m = fmap (configure c) m
  dimensions = foldMap dimensions


--
-- * Other functions
--

-- | Merge second mask into the first.
mergeMask :: Mask -> Mask -> Mask
-- If there's no error in one of the masks, then just choose the other mask
mergeMask (One Nothing) m = m
mergeMask m (One Nothing) = m
-- An error in the original mask dominates the mergee
mergeMask e@(One (Just _)) _ = e
-- Merge a single error with the leaves of the choice tree
mergeMask (Chc d l r ) e@(One (Just _)) = Chc d (mergeMask l e) (mergeMask r e)
-- Merge errors in variational contexts
mergeMask m@(Chc d l r) m'@(Chc d' l' r')
  -- If the dimensions are equiv, merge both sides with each other
  | d |=|   d'  = Chc d (mergeMask l l') (mergeMask r r')
  -- If d |<=| d', merge new mask only into left choice
  | d |<=|  d' = Chc d (mergeMask l m') r
  -- If d |!<=| d', merge new mask only into right choice
  | d |!<=| d' = Chc d l (mergeMask r m')
  -- If d |=>| d', merge original mask into left choice of new mask
  | d |=>|  d' = Chc d' (mergeMask m l') r'
  -- If d |=>!| d', merge original mask into right choice of new mask
  | d |=>!| d' = Chc d' l' (mergeMask m r')
  -- Otherwise, merge new mask into both sides of choice
  | otherwise  = Chc d (mergeMask l m') (mergeMask r m')

-- | Turns a variational value into a variational optional value
toVMaybe :: V a -> V (Maybe a)
toVMaybe = fmap Just
