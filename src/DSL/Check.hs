
module DSL.Check where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map.Strict as Map

import DSL.Env
import DSL.Expr
import DSL.Predicate
import DSL.SAT
import DSL.Type


--
-- * Type Checking
--

-- | The typing environment maps variables to refined types.
type TEnv = Env (Type Refined)

-- | A monad to support the typing relation. The environment in the
--   reader monad tracks non-linear assumptions, while the environment
--   in the state monad tracks linear assumptions.
type TypeM = ReaderT TEnv (StateT TEnv (Either String))

-- | Lookup a non-linear assumption.
lookRef :: Var -> TypeM (Type Refined)
lookRef v = do
    m <- ask
    case Map.lookup v m of
      Just t -> return t
      _      -> fail ("unbound non-linear variable: " ++ v)

-- | Lookup and consume a linear assumption.
lookUse :: Var -> TypeM (Type Refined)
lookUse v = do
    m <- get
    case Map.lookup v m of
      Just t -> put (Map.delete v m) >> return t
      _      -> fail ("unavailable linear variable: " ++ v)

-- | Push a linear assumption onto the typing environment, then a run a typing
--   computation and check to see whether that assumption was consumed.
--   If so, return the result. Otherwise, fail.
typeWith :: Var -> Type Refined -> TypeM a -> TypeM a
typeWith v t ma = do
    old <- gets (Map.lookup v)
    modify (Map.insert v t)
    result <- ma
    unused <- gets (Map.member v)
    when unused (fail ("unused linear variable: " ++ v))
    case old of
      Just t -> modify (Map.insert v t)
      _      -> return ()
    return result
