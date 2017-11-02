module DSL.Profile where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import DSL.Types
import DSL.Effect
import DSL.Environment
import DSL.Expression
import DSL.Path
import DSL.Resource


--
-- * Resource Profiles
--


-- | Construct a profile from an argument list and an association list of effects.
profile :: [Param] -> [(Path, [Effect])] -> Profile
profile xs = Profile xs . envFromList

-- | Load a profile by resolving all of its effects.
loadProfile :: MonadEval m => Profile -> [Expr] -> m ()
loadProfile (Profile xs effs) args =
    withArgs xs args (envMapM_ (mapM_ . resolve) effs)
  where
    resolve path eff = do
      rID <- getResID path
      resolveEffect rID eff
