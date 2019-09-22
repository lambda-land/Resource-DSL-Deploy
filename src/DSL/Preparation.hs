-- | Prepare a DSL program for evaluation.
module DSL.Preparation where

import Data.Data (Typeable)
import GHC.Generics (Generic)

import Control.Monad.Reader
import Data.Set (Set)
import qualified Z3.Monad as Z3

import DSL.Boolean
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

