module DSL.Profile where

import DSL.Types
import DSL.Effect
import DSL.Environment
import DSL.Expression
import DSL.Resource
import DSL.SegList
import DSL.SAT


--
-- * Resource Profiles
--


-- | Construct a profile from an argument list and an association list of effects.
profile :: [Param] -> [(Path, SegList Effect)] -> Profile
profile xs = Profile xs . envFromList

-- | Load a profile by resolving all of its effects.
loadProfile :: MonadEval m => Profile -> [V Expr] -> m ()
loadProfile (Profile xs effs) args =
    withArgs xs args (envMapM_ (segMapM_ . resolve) effs)
  where
    resolve path eff = do
      rID <- getResID path
      resolveEffect rID eff

selectProfile :: BExpr -> Profile -> Profile
selectProfile d (Profile ps es) = Profile (map (selectParam d) ps) (fmap (selectEffs d) es)

selectEffs :: BExpr -> SegList Effect -> SegList Effect
selectEffs _ [] = []
selectEffs d ((Elems xs):ys) = Elems (map (selectEff d) xs) : selectEffs d ys
selectEffs d ((Split d' l r):ys) | d |=>| d' = selectEffs d l ++ selectEffs d ys
                                 | d |=>!| d' = selectEffs d r ++ selectEffs d ys
                                 | otherwise = Split d' (selectEffs d l) (selectEffs d r) : selectEffs d ys
