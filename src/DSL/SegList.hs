module DSL.SegList where

import DSL.Types
import DSL.SAT
import DSL.Resource

segMapM_ :: (MonadEval m) => (a -> m b) -> SegList a -> m ()
segMapM_ f ((Elems ys):xs) = mapM_ f ys >> segMapM_ f xs
segMapM_ f ((Split d l r):xs) = vHandleUnit d (segMapM_ f) l r >> segMapM_ f xs
segMapM_ _ [] = return ()

segSetInsert :: Maybe BExpr -> a -> SegList a -> SegList a
segSetInsert Nothing a [] = [Elems [a]]
segSetInsert Nothing a ((Elems as):xs) = (Elems (a:as)):xs
segSetInsert Nothing a (x:xs) = x:(segSetInsert Nothing a xs)
segSetInsert (Just d) a [] = [Split d [Elems [a]] []]
segSetInsert (Just d) a ((Split d' l r):xs)
  | d |=| d' = (Split d' (segSetInsert Nothing a l) r):xs
  | d |!=| d' = (Split d' l (segSetInsert Nothing a r)):xs
  | d |<=| d' = (Split d' (segSetInsert (Just d) a l) r):xs
  | d |!<=| d' = (Split d' l (segSetInsert (Just d) a r)):xs
  | otherwise = (Split d' l r):(segSetInsert (Just d) a xs)
segSetInsert d a (x:xs) = x:(segSetInsert d a xs)
