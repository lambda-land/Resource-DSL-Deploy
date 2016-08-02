{-# LANGUAGE
      DeriveGeneric,
      FlexibleContexts
  #-}

module DSL.Value where

import Control.Monad (liftM2)
import Control.Monad.Except (MonadError,throwError)

import GHC.Generics (Generic)

import DSL.Predicate
import DSL.Primitive


--
-- * Values
--

-- | Values.
data Value
     = Lit_ PVal
     | Tup_ Value Value
     | Chc_ Pred Value Value
  deriving (Eq,Generic,Show)

-- | Apply a primitive unary function to a value.
applyPrim1 :: MonadError String m => Op1 -> Value -> m Value
applyPrim1 o (Lit_ v)     = fmap Lit_ (primOp1 o v)
applyPrim1 o (Chc_ p l r) = liftM2 (Chc_ p) (applyPrim1 o l) (applyPrim1 o r)
applyPrim1 o v = throwError $ "applyPrim1: type error applying op "
                              ++ show o ++ " to value " ++ show v

-- | Apply a primitive binary function to two values.
applyPrim2 :: MonadError String m => Op2 -> Value -> Value -> m Value
applyPrim2 o (Lit_ l) (Lit_ r) = fmap Lit_ (primOp2 o l r)
applyPrim2 o (Chc_ p ll lr) r  = liftM2 (Chc_ p) (applyPrim2 o ll r) (applyPrim2 o lr r)
applyPrim2 o l (Chc_ p rl rr)  = liftM2 (Chc_ p) (applyPrim2 o l rl) (applyPrim2 o l rr)
applyPrim2 o l r = throwError $ "applyPrim2: type error applying op "
                                ++ show o ++ " to value " ++ show l ++ " and " ++ show r

-- | Apply the fst function to a value.
applyFst :: MonadError String m => Value -> m Value
applyFst (Tup_ l _)   = return l
applyFst (Chc_ p l r) = liftM2 (Chc_ p) (applyFst l) (applyFst r)
applyFst v = throwError $ "applyFst: type error: " ++ show v

-- | Apply the snd function to a value.
applySnd :: MonadError String m => Value -> m Value
applySnd (Tup_ _ r)   = return r
applySnd (Chc_ p l r) = liftM2 (Chc_ p) (applySnd l) (applySnd r)
applySnd v = throwError $ "applySnd: type error: " ++ show v
