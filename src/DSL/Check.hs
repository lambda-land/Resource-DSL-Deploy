
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

-- | The typing environment maps variables to refined type schemas.
type TEnv = Env (Schema Refined)

-- | A monad to support the typing relation. The environment in the
--   reader monad tracks non-linear assumptions, while the environment
--   in the state monad tracks linear assumptions.
type TypeM = ReaderT TEnv (StateT TEnv (Either String))

-- | Lookup a non-linear assumption.
lookRef :: Var -> TypeM (Schema Refined)
lookRef v = do
    m <- ask
    case Map.lookup v m of
      Just t -> return t
      _      -> fail ("unbound non-linear variable: " ++ v)

-- | Lookup and consume a linear assumption.
lookUse :: Var -> TypeM (Schema Refined)
lookUse v = do
    m <- get
    case Map.lookup v m of
      Just t -> put (Map.delete v m) >> return t
      _      -> fail ("unavailable linear variable: " ++ v)

-- | Push a linear assumption onto the typing environment, then a run a typing
--   computation and check to see whether that assumption was consumed.
--   If so, return the result. Otherwise, fail.
typeWith :: Var -> Schema Refined -> TypeM a -> TypeM a
typeWith v s ma = do
    old <- gets (Map.lookup v)
    modify (Map.insert v s)
    result <- ma
    unused <- gets (Map.member v)
    when unused (fail ("unused linear variable: " ++ v))
    case old of
      Just s -> modify (Map.insert v s)
      _      -> return ()
    return result

-- | Check whether two types are compatible.
check :: (Eq t, Show t) => String -> Type t -> Type t -> TypeM ()
check s t u | t == u    = return ()
            | otherwise = fail ("incompatible types in " ++ s ++ ": "
                                ++ show t ++ " and " ++ show u)

-- | Print an error message for failed expectation.
expected :: Show t => String -> Type t -> TypeM a
expected s t = fail ("expected " ++ s ++ ", got: " ++ show t)

-- | Typing relation. Returns the type schema of a term and the updated
--   environment or else reports a type error.
typeOf :: Expr Refined -> TypeM (Schema Refined)
    -- environment lookup
typeOf (Ref x)      = lookRef x
typeOf (Use x)      = lookUse x
    -- introduction forms
-- typeOf (Abs x t b)  = typeWith x t (fmap (t :->) (typeOf b))
-- typeOf (Free e)     = liftM Bang (typeOf e)
