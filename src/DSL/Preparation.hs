{-# LANGUAGE UndecidableInstances #-}

-- | Prepare a DSL program for evaluation.
module DSL.Preparation where

import Data.Data (Typeable)
import GHC.Generics (Generic)

import Control.Monad (liftM2)
import Control.Monad.Reader
import qualified Z3.Monad as Z3

import DSL.Condition
import DSL.SAT
import DSL.Types


--
-- * Prepare a program for evaluation
--

-- ** Preparation monad

-- | Reader context for preparation
data PrepCtx = PCtx {
  z3Solver  :: Z3.Solver,  -- ^ solver reference
  z3Context :: Z3.Context, -- ^ solver context
  symEnv    :: SymEnv      -- ^ symbol environment
} deriving (Eq,Generic,Show,Typeable)

-- | Requirements of a preparation monad.
type MonadPrep m = (MonadReader PrepCtx m, Z3.MonadZ3 m)

-- | A monad for preparing variational values for evaluation.
newtype PrepM a = PrepM {
  unPrepM :: ReaderT PrepCtx IO a
}

-- | Execute a computation on the given inputs with initialized contexts.
runPrep :: PrepCtx -> PrepM a -> IO a
runPrep ctx (PrepM mx) = runReaderT mx ctx

instance Functor PrepM where
  fmap f (PrepM mx) = PrepM (fmap f mx)

instance Applicative PrepM where
  pure a = PrepM (pure a)
  (<*>) = ap

instance Monad PrepM where
  PrepM mx >>= f = PrepM (mx >>= unPrepM . f)

instance MonadIO PrepM where
  liftIO mx = PrepM (liftIO mx)

instance MonadReader PrepCtx PrepM where
  ask = PrepM ask
  local f (PrepM mx) = PrepM (local f mx)

instance (Applicative m, Monad m, MonadIO m, MonadReader PrepCtx m) => Z3.MonadZ3 m where
  getSolver = asks z3Solver
  getContext = asks z3Context


-- ** Operations

-- | Prepare a condition for evaluation.
prepareCond :: MonadPrep m => Cond -> m Cond
prepareCond (Cond e _) = do
    let e' = shrinkBExpr e
    m <- asks symEnv
    s <- symBExpr m e'
    return (Cond e' (Just s))

-- | Prepare a variational value for evaluation.
prepareV :: MonadPrep m => V a -> m (V a)
prepareV (One a)     = return (One a)
prepareV (Chc c l r) = do
    c' <- prepareCond c
    case condExpr c' of
      BLit True  -> prepareV l
      BLit False -> prepareV r
      _ -> liftM2 (Chc c') (prepareV l) (prepareV r)
