module DSL.Profile where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Data.List (union)
import Control.Monad.Catch (MonadThrow)

import DSL.Effect
import DSL.Environment
import DSL.Expression
import DSL.Predicate
import DSL.Resource


--
-- * Resource Profiles
--

-- | Resource profile: a parameterized account of all of the resource effects
--   of a program or component.
data Profile = Profile [Var] (HEnv [Effect])
  deriving (Data,Eq,Generic,Read,Show,Typeable)

-- | Load a profile by resolving all of its effects.
loadProfile :: MonadEval m => Profile -> [Expr] -> m ()
loadProfile (Profile xs effs) args = withArgs xs args undefined

-- | Compose two resource profiles. Merges parameters by name.
composeProfiles :: MonadThrow m => Profile -> Profile -> m Profile
composeProfiles (Profile ps1 h1) (Profile ps2 h2) =
    fmap (Profile ps) (henvUnionWith cat h1 h2)
  where
    ps = ps1 `union` ps2
    cat l r = return (l ++ r)
