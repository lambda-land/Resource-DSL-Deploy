module DSL.Profile where

import DSL.Types
import DSL.Effect
import DSL.Environment
import DSL.Expression
import DSL.Resource
import DSL.SegList


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
