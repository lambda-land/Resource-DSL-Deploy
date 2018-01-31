module DSL.SegList where

import DSL.Types
import DSL.Resource

segMapM_ :: (MonadEval m) => (a -> m b) -> SegList a -> m ()
segMapM_ f ((Elems ys):xs) = mapM_ f ys >> segMapM_ f xs
segMapM_ f ((Split d l r):xs) = vHandleUnit d (segMapM_ f) l r >> segMapM_ f xs
segMapM_ _ [] = return ()
