module DSL.Profile where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Data.List (union)
import Control.Monad.Catch (MonadCatch,MonadThrow)

import DSL.Effect
import DSL.Environment
import DSL.Expression
import DSL.Name
import DSL.Primitive
import DSL.Resource


--
-- * Resource Profiles
--

-- | Resource profile: a parameterized account of all of the resource effects
--   of a program or component.
data Profile = Profile [Param] (Env ResID [Effect])
  deriving (Data,Eq,Generic,Read,Show,Typeable)

-- | Construct a profile from an argument list and an association list of effects.
profile :: [Param] -> [(ResID, [Effect])] -> Profile
profile xs = Profile xs . envFromList

-- | Load a profile by resolving all of its effects.
loadProfile :: MonadEval m => Profile -> [Expr] -> m ()
loadProfile (Profile xs effs) args =
    withArgs xs args (envMapM_ (mapM_ . resolveEffect) effs)

-- | Compose two resource profiles. Merges parameters by name.
composeProfiles :: Profile -> Profile -> Profile
composeProfiles (Profile ps1 h1) (Profile ps2 h2) =
    Profile (union ps1 ps2) (envUnionWith (++) h1 h2)

instance MergeDup Profile where
  mergeDup = composeProfiles
