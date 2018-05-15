module DSL.V where

import DSL.Types
import DSL.SAT

class Select a where
  sel :: BExpr -> a -> a
  conf :: BExpr -> a -> a

instance Select a => Select (V a) where
  sel d (One a) = One (sel d a)
  sel d (Chc d' l r) | d |=>|  d' = sel d l
                     | d |=>!| d' = sel d r
                     | otherwise  = Chc d' (sel d l) (sel d r)

  conf d (One a) = One (conf d a)
  conf d (Chc d' l r) | d |=>| d' = conf d l
                      | otherwise = conf d r

instance Select PVal where
  sel _ = id
  conf _ = id

instance Select PType where
  sel _ = id
  conf _ = id

instance Select Param where
  sel d (Param v t) = Param v (sel d t)
  conf d (Param v t) = Param v (conf d t)

instance Select Fun where
  sel d (Fun p e) = Fun (sel d p) (sel d e)
  conf d (Fun p e) = Fun (conf d p) (conf d e)

instance Select Expr where
  sel d (Lit pv) = Lit (sel d pv)
  sel d (P1 o e) = P1 o (sel d e)
  sel d (P2 o e1 e2) = P2 o (sel d e1) (sel d e2)
  sel d (P3 o e1 e2 e3) = P3 o (sel d e1) (sel d e2) (sel d e3)
  sel _ e = e

  conf d (Lit pv) = Lit (conf d pv)
  conf d (P1 o e) = P1 o (conf d e)
  conf d (P2 o e1 e2) = P2 o (conf d e1) (conf d e2)
  conf d (P3 o e1 e2 e3) = P3 o (conf d e1) (conf d e2) (conf d e3)
  conf _ e = e

instance Select a => Select (SegList a) where
  sel _ [] = []
  sel d ((Elems xs):ys) = Elems (map (sel d) xs) : sel d ys
  sel d ((Split d' l r):ys) | d |=>| d' = sel d l ++ sel d ys
                            | d |=>!| d' = sel d r ++ sel d ys
                            | otherwise = Split d' (sel d l) (sel d r) : sel d ys

  conf _ [] = []
  conf d ((Elems xs):ys) = Elems (map (conf d) xs) : conf d ys
  conf d ((Split d' l r):ys) | d |=>| d' = conf d l ++ conf d ys
                             | otherwise = conf d r ++ conf d ys

instance Select Effect where
  sel d (Create e) = Create (sel d e)
  sel d (Check f) = Check (sel d f)
  sel d (Modify f) = Modify (sel d f)
  sel _ Delete = Delete

  conf d (Create e) = Create (conf d e)
  conf d (Check f) = Check (conf d f)
  conf d (Modify f) = Modify (conf d f)
  conf _ Delete = Delete

instance Select Stmt where
  sel d (Do p e) = Do p (sel d e)
  sel d (If c t e) = If (sel d c) (sel d t) (sel d e)
  sel d (In p blk) = In p (sel d blk)
  sel d (For v e blk) = For v (sel d e) (sel d blk)
  sel d (Let v e blk) = Let v (sel d e) (sel d blk)
  sel d (Load e es) = Load (sel d e) (map (sel d) es)

  conf d (Do p e) = Do p (conf d e)
  conf d (If c t e) = If (conf d c) (conf d t) (conf d e)
  conf d (In p blk) = In p (conf d blk)
  conf d (For v e blk) = For v (conf d e) (conf d blk)
  conf d (Let v e blk) = Let v (conf d e) (conf d blk)
  conf d (Load e es) = Load (conf d e) (map (conf d) es)

instance Select Model where
  sel d (Model ps blk) = Model (map (sel d) ps) (sel d blk)
  conf d (Model ps blk) = Model (map (conf d) ps) (conf d blk)

instance Select Profile where
  sel d (Profile ps es) = Profile (map (sel d) ps) (fmap (sel d) es)
  conf d (Profile ps es) = Profile (map (conf d) ps) (fmap (conf d) es)

instance Select Entry where
  sel d (ProEntry p) = ProEntry (sel d p)
  sel d (ModEntry m) = ModEntry (sel d m)

  conf d (ProEntry p) = ProEntry (conf d p)
  conf d (ModEntry m) = ModEntry (conf d m)

instance Select v => Select (Env k v) where
  sel d env = fmap (sel d) env
  conf d env = fmap (conf d) env

instance Select a => Select (Maybe a) where
  sel d m = fmap (sel d) m
  conf d m = fmap (conf d) m

-- select takes a dimension and eliminates all choices implied by it or its opposite
select :: BExpr -> V a -> V a
select _ (One a) = One a
select d (Chc d' l r) | d |=>|  d' = select d l
                      | d |=>!| d' = select d r
                      | otherwise  = Chc d' (select d l) (select d r)

-- same as select except choices not selected on are assumed to be true
configure :: BExpr -> V a -> a
configure _ (One a) = a
configure d (Chc d' l r) | d |=>| d' = configure d l
                         | otherwise = configure d r

-- | Merge second mask into the first
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
