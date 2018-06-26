module DSL.VEnv where

import Data.SBV (bnot)
import Control.Monad.Except

import DSL.Types
import DSL.V
import DSL.Resource
import DSL.Environment
import DSL.SAT

-- | Look up a resource in a variational context, returning a
--   variational value. Note that it does not throw errors for
--   nonexistent resources, so error handling should be handled
--   separately.
vLookup' :: (MonadEval m, Ord k) => k -> VEnv k v -> m (VOpt v)
vLookup' k env | Just v <- envLookup' k env = do
                   c <- getVCtx
                   return $ select c v
               | otherwise = return (One Nothing)

-- | Check if a value exists in all variants, throwing errors
--   for variants where it does not exist.
checkExists :: MonadEval m => Error -> VOpt v -> m ()
checkExists e (One Nothing) = vError e
checkExists _ (One (Just _)) = return ()
checkExists e (Chc d l r) = vHandleUnit d (checkExists e) l r

-- | Look up a value in a variational environment, throwing an error if
--   does not exist in a particular variant.
vLookup :: (MonadEval m, Ord k) => Error -> k -> VEnv k v -> m (VOpt v)
vLookup e k env = do
  v <- vLookup' k env
  checkExists e v
  return v

vDelete' :: (MonadEval m) => Error -> BExpr -> VOpt v -> m (VOpt v)
vDelete' e _ (One Nothing) = vError e
vDelete' _ d v@(One (Just _)) = return (Chc d (One Nothing) v)
vDelete' e d (Chc d' l r)
                          | d |=>| d' = do
                              l' <- vDelete' e d l
                              return (Chc d' l' r)
                          | d |=>!| d' = do
                              r' <- vDelete' e d r
                              return (Chc d' l r')
                          | d |<=| d' = do
                              let r' = vMove (bnot d') (vDelete' e d r)
                              vMove d' (checkExists e l) `catchError` (\e -> do
                                  r' `catchError` (\e' -> throwError (Chc d' e e'))
                                  return ()
                                )
                              r'' <- r' `catchError` (\_ -> return (One Nothing))
                              return (Chc d' (One Nothing) r'')
                          | d |!<=| d' = do
                              let l' = vMove d' (vDelete' e d l)
                              vMove (bnot d') (checkExists e r) `catchError` (\e -> do
                                  l' `catchError` (\e' -> throwError (Chc d' e' e))
                                  return ()
                                )
                              l'' <- l' `catchError` (\_ -> return (One Nothing))
                              return (Chc d' l'' (One Nothing))
                          | otherwise = vHandle d' (vDelete' e d) l r

vDelete :: (MonadEval m, Ord k) => Error -> k -> VEnv k v -> m (VEnv k v)
vDelete e k env | Just v <- envLookup' k env = do
                    c <- getVCtx
                    v' <- vDelete' e c v
                    return (envExtend k v' env)
                | otherwise = vError e

checkNExists :: MonadEval m => Error -> VOpt v -> m ()
checkNExists e (One (Just _)) = vError e
checkNExists _ (One Nothing) = return ()
checkNExists e (Chc d l r) = vHandleUnit d (checkNExists e) l r

vCreate' :: (MonadEval m) => Error -> BExpr -> VOpt v -> VOpt v -> m (VOpt v)
vCreate' _ d v@(One Nothing) new = return (Chc d new v)
vCreate' e _ (One (Just _)) _ = vError e
vCreate' e d (Chc d' l r) new
                              | d |=>| d' = do
                                  l' <- vCreate' e d l new
                                  return (Chc d' l' r)
                              | d |=>!| d' = do
                                  r' <- vCreate' e d r new
                                  return (Chc d' l r')
                              | d |<=| d' = do
                                  let r' = vMove (bnot d') (vCreate' e d r new)
                                  vMove d' (checkNExists e l) `catchError` (\e -> do
                                      r' `catchError` (\e' -> throwError (Chc d' e e'))
                                      return ()
                                    )
                                  r'' <- r' `catchError` (\_ -> return (One Nothing))
                                  return (Chc d' new r'')
                              | d |!<=| d' = do
                                  let l' = vMove d' (vCreate' e d l new)
                                  vMove (bnot d') (checkNExists e r) `catchError` (\e -> do
                                      l' `catchError` (\e' -> throwError (Chc d' e' e))
                                      return ()
                                    )
                                  l'' <- l' `catchError` (\_ -> return (One Nothing))
                                  return (Chc d' l'' new)
                              | otherwise = vHandle d' (\v -> vCreate' e d v new) l r

lookup' :: (Ord k) => k -> VEnv k v -> VOpt v
lookup' k env | Just v <- envLookup' k env = v
              | otherwise = One Nothing

vCreate :: (MonadEval m, Ord k) => Error -> k -> VEnv k v -> VOpt v -> m (VEnv k v)
vCreate e k env new = do
  c <- getVCtx
  v <- vCreate' e c (lookup' k env) new
  return (envExtend k v env)

vModify'' :: MonadEval m => Error -> (VOpt v -> m (VOpt v)) -> VOpt v -> m (VOpt v)
vModify'' e f v = do
  checkExists e v
  f v

vModify' :: MonadEval m => Error -> BExpr -> (VOpt v -> m (VOpt v)) -> VOpt v -> m (VOpt v)
vModify' e _ _ (One Nothing) = vError e
vModify' _ d f v@(One (Just _)) = do
  v' <- f v
  return (Chc d v' v)
vModify' e d f (Chc d' l r)
                            | d |=>| d' = do
                                l' <- vModify' e d f l
                                return (Chc d' l' r)
                            | d |=>!| d' = do
                                r' <- vModify' e d f r
                                return (Chc d' l r')
                            | d |<=| d' = do
                                let r' = vMove (bnot d') (vModify' e d f r)
                                l' <- vMove d' (vModify'' e f l) `catchError` (\e -> do
                                    r' `catchError` (\e' -> throwError (Chc d' e e'))
                                    return (One Nothing)
                                  )
                                r'' <- r' `catchError` (\_ -> return (One Nothing))
                                return (Chc d' l' r'')
                            | d |!<=| d' = do
                                let l' = vMove d' (vModify' e d f l)
                                r' <- vMove (bnot d') (vModify'' e f l) `catchError` (\e -> do
                                    l' `catchError` (\e' -> throwError (Chc d' e' e))
                                    return (One Nothing)
                                  )
                                l'' <- l' `catchError` (\_ -> return (One Nothing))
                                return (Chc d' l'' r')
                            | otherwise = vHandle d' (vModify' e d f) l r

vModify :: (MonadEval m, Ord k) => Error -> k -> VEnv k v -> (VOpt v -> m (VOpt v)) -> m (VEnv k v)
vModify e k env f = do
  c <- getVCtx
  v <- vModify' e c f (lookup' k env)
  return (envExtend k v env)
