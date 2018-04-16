module DSL.Effect where


import DSL.Types
import DSL.Expression
import DSL.Resource
import DSL.VEnv


--
-- * Resource Effects
--



-- ** Resolution


checkResult :: MonadEval m => (EffectErrorKind -> Error) -> Value -> m ()
checkResult _ (One Nothing) = return ()
checkResult _ (One (Just (B True))) = return ()
checkResult f (One (Just (B False))) = vError (f CheckFailure)
checkResult f (One (Just _)) = vError (f CheckTypeError)
checkResult f (Chc d l r) = vHandleUnit d (checkResult f) l r

-- | Execute the effect on the given resource environment.
resolveEffect :: MonadEval m => ResID -> Effect -> m ()
-- create
resolveEffect rID eff@(Create expr) = do
  v <- unVM $ evalExprV expr
  env <- getResEnv
  env' <- vCreate (EffE $ EffectError eff ResourceAlreadyExists rID Nothing) rID env v
  updateResEnv (\_ -> env')
-- check
resolveEffect rID eff@(Check fun) = do
  env <- getResEnv
  v <- vLookup (EffE $ EffectError eff NoSuchResource rID Nothing) rID env
  u <- evalFun fun v
  checkResult (\t -> EffE $ EffectError eff t rID (Just v)) u
-- modify
resolveEffect rID eff@(Modify fun) = do
  env <- getResEnv
  env' <- vModify (EffE $ EffectError eff NoSuchResource rID Nothing) rID env (evalFun fun)
  updateResEnv (\_ -> env')
-- delete
resolveEffect rID Delete = do
  env <- getResEnv
  env' <- vDelete (EffE $ EffectError Delete NoSuchResource rID Nothing) rID env
  updateResEnv (\_ -> env')

selectEff :: BExpr -> Effect -> Effect
selectEff d (Create e) = Create (selectExpr d e)
selectEff d (Check f) = Check (selectFun d f)
selectEff d (Modify f) = Modify (selectFun d f)
selectEff _ Delete = Delete
