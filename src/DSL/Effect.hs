module DSL.Effect where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Control.Monad (unless,when)
import Control.Monad.Catch (Exception,throwM)

import DSL.Environment
import DSL.Expression
import DSL.Resource
import DSL.Value


--
-- * Resource Effects
--

-- ** Type

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

-- | Check that a resource exists at the given path;
--   if not, throw an error for the given effect.
checkExists :: MonadEval m => Effect -> Path -> m ()
checkExists eff path = do
    exists <- queryResEnv (envHas path)
    unless exists $
      throwM (EffectError eff NoSuchResource path Nothing)

-- | Execute the effect on the given resource environment.
resolveEffect :: MonadEval m => Path -> Effect -> m ()
-- create
resolveEffect path eff@(Create expr) = do
    exists <- queryResEnv (envHas path)
    when exists $
      throwM (EffectError eff ResourceAlreadyExists path Nothing)
    val <- evalExpr expr
    updateResEnv (envExtend path val)
-- check
resolveEffect path eff@(Check fun) = do
    checkExists eff path
    val <- getResEnv >>= envLookup path
    result <- evalFun fun val
    case valIsTrue result of
      Just True  -> return ()
      Just False -> throwM (EffectError eff CheckFailure path (Just val))
      Nothing    -> throwM (EffectError eff CheckTypeError path (Just val))
-- modify
resolveEffect path eff@(Modify fun) = do
    checkExists eff path
    new <- getResEnv >>= envLookup path >>= evalFun fun
    updateResEnv (envExtend path new)
-- delete
resolveEffect path Delete = do
    checkExists Delete path
    updateResEnv (envDelete path)
