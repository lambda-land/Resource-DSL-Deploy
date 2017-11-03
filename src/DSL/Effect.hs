module DSL.Effect where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Control.Monad (unless,when)
import Control.Monad.Catch (Exception,throwM)

import DSL.Types
import DSL.Environment
import DSL.Expression
import DSL.Path
import DSL.Resource
import DSL.Primitive


--
-- * Resource Effects
--



-- ** Resolution

-- | Check that a resource exists at the given resource ID;
--   if not, throw an error for the given effect.
checkExists :: MonadEval m => Effect -> ResID -> m ()
checkExists eff rID = do
    exists <- queryResEnv (envHas rID)
    unless exists $
      throwM (EffectError eff NoSuchResource rID Nothing)

-- | Execute the effect on the given resource environment.
resolveEffect :: MonadEval m => ResID -> Effect -> m ()
-- create
resolveEffect rID eff@(Create expr) = do
    exists <- queryResEnv (envHas rID)
    when exists $
      throwM (EffectError eff ResourceAlreadyExists rID Nothing)
    val <- evalExpr expr
    updateResEnv (envExtend rID val)
-- check
resolveEffect rID eff@(Check fun) = do
    checkExists eff rID
    val <- getResEnv >>= envLookup rID
    result <- evalFun fun val
    case result of
      B True  -> return ()
      B False -> throwM (EffectError eff CheckFailure rID (Just val))
      _       -> throwM (EffectError eff CheckTypeError rID (Just val))
-- modify
resolveEffect rID eff@(Modify fun) = do
    checkExists eff rID
    new <- getResEnv >>= envLookup rID >>= evalFun fun
    updateResEnv (envExtend rID new)
-- delete
resolveEffect rID Delete = do
    checkExists Delete rID
    updateResEnv (envDelete rID)
