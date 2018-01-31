module DSL.V where

import DSL.Types
import DSL.SAT

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
