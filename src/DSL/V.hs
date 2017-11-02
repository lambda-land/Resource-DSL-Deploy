{-# LANGUAGE ExistentialQuantification #-}

module DSL.V where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)
import Control.Exception

import DSL.Predicate (BExpr)



data V a = One a | Chc BExpr (V a) (V a)
  deriving (Data,Eq,Generic,Read,Show,Typeable)

instance Functor V where
  fmap f (One v) = One . f $ v
  fmap f (Chc d l r) = Chc d (fmap f l) (fmap f r)

instance Applicative V where
  pure = One
  (One f) <*> v = fmap f v
  (Chc d f g) <*> v = Chc d (f <*> v) (g <*> v)

instance Monad V where
  (One v) >>= f = f v
  (Chc d l r) >>= f = Chc d (l >>= f) (r >>= f)

data ExiErr = forall e . Exception e => ExiErr e

instance Show ExiErr where
  show (ExiErr e) = show e

type VErr = V ExiErr

instance Exception VErr

type Mask = V (Maybe ExiErr)
