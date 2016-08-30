{-# LANGUAGE UndecidableInstances #-}

module DSL.Resource where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Control.Monad        (unless,when)
import Control.Monad.Catch  (Exception,MonadCatch,MonadThrow,throwM)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Writer (MonadWriter,tell)

import Data.List (union)

import DSL.Environment
import DSL.Expression
import DSL.Predicate
import DSL.Primitive
import DSL.Value
-- import DSL.Type


--
-- * Resource Effects
--

-- ** Types

-- | Resource environment.
type REnv = HEnv Value

-- | An effect on a particular resource.
data Effect
     = Create Expr
     | Check  Fun
     | Modify Fun
     | Delete
  deriving (Data,Eq,Generic,Read,Show,Typeable)


-- ** Errors

-- | Kinds of errors that can occur when resolving or combining an effect.
data EffectErrorKind
     = CheckFailure
     | CheckTypeError
     | NoSuchResource
     | ResourceAlreadyExists
     | EffectConflict
  deriving (Data,Eq,Generic,Read,Show,Typeable)

-- | An error resulting from applying a resource effect.
data EffectError = EffectError {
     errorEffect :: Effect,
     errorKind   :: EffectErrorKind,
     errorPath   :: Path,
     errorValue  :: Maybe Value
} deriving (Data,Eq,Generic,Read,Show,Typeable)

instance Exception EffectError


-- ** Resolution

-- | A monad that supports resolving effects. Just a synonym for convenience.
class (MonadCatch m, MonadReader (Env Value) m) => MonadEffect m
instance (MonadCatch m, MonadReader (Env Value) m) => MonadEffect m

-- | Check that a resource exists at the given path;
--   if not, throw an error for the given effect.
checkExists :: MonadEffect m => Effect -> Path -> REnv -> m ()
checkExists eff path env = do
    exists <- henvHas path env
    unless exists $
      throwM (EffectError eff NoSuchResource path Nothing)

-- | Execute the effect on the given resouce environment.
resolveEffect :: MonadEffect m => Path -> Effect -> REnv -> m REnv
-- create
resolveEffect path eff@(Create expr) env = do
    exists <- henvHas path env
    when exists $
      throwM (EffectError eff ResourceAlreadyExists path Nothing)
    val <- evalExpr expr
    henvExtend path val env
-- check
resolveEffect path eff@(Check fun) env = do
    checkExists eff path env
    val <- henvLookup path env
    result <- evalFun fun val
    case valIsTrue result of
      Just True  -> return env
      Just False -> throwM (EffectError eff CheckFailure path (Just val))
      Nothing    -> throwM (EffectError eff CheckTypeError path (Just val))
-- modify
resolveEffect path eff@(Modify fun) env = do
    checkExists eff path env
    old <- henvLookup path env
    new <- evalFun fun old
    henvExtend path new env
-- delete
resolveEffect path Delete env = do
    checkExists Delete path env
    henvDelete path env


--
-- * Resource Profiles
--

-- | Resource profile: a parameterized account of all of the resource effects
--   of a program or component.
data Profile = Profile [Var] (HEnv [Effect])
  deriving (Eq,Generic,Show) 

-- | Compose two resource profiles. Merges parameters by name.
composeProfiles :: MonadThrow m => Profile -> Profile -> m Profile
composeProfiles (Profile ps1 h1) (Profile ps2 h2) =
    fmap (Profile ps) (henvUnionWith cat h1 h2)
  where
    ps = ps1 `union` ps2
    cat l r = return (l ++ r)
