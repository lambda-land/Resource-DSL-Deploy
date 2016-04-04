
module DSL.Check where

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

-- | A monad to support the typing relation.
type TypeM = EnvM (Schema Refined)

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
