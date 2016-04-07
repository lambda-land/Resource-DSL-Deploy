
module DSL.Check where

import Control.Monad.Except (throwError)
import Control.Monad.State
import qualified Data.Map as Map

import DSL.Env
import DSL.Expr
import DSL.Predicate
import DSL.Primitive
import DSL.Row
import DSL.SAT
import DSL.Type


--
-- * Type Checking
--

-- | A monad to support the typing relation.
type TypeM = EnvM (Schema Refined)

-- | Check whether a value has a given base type.
checkBase :: Expr Refined -> Type Refined -> Bool
checkBase Unit  (Base (Simple TUnit))      = True
checkBase (B _) (Base (Simple TBool))      = True
checkBase (I _) (Base (Simple TInt))       = True
checkBase (B b) (Base (Refined TBool v p)) = sat ((BRef v <=> BLit b) &&& p)
checkBase (I i) (Base (Refined TBool v p)) = sat ((IRef v .== ILit i) &&& p)
checkBase (Free e) (Bang t) = checkBase e t
checkBase _ _ = False
  
-- | Check whether a value in a row has a given base type.
checkLabel :: Label -> Type Refined -> Row (Expr Refined) -> Bool
checkLabel l t = maybe False (flip checkBase t) . Map.lookup l

-- | Check whether a record value has a given record type. Temporary solution
--   to enable checking outputs against mission requirements.
checkRec :: Expr Refined -> Schema Refined -> Bool
checkRec (Rec val) (Forall _ (TRec typ _)) =
    Map.foldrWithKey (\l t b -> b && checkLabel l t val) True typ
checkRec _ _ = False

-- | Check whether two types are compatible.
check :: (Eq t, Show t) => String -> Type t -> Type t -> TypeM ()
check s t u | t == u    = return ()
            | otherwise = throwError ("incompatible types in " ++ s ++ ": "
                                      ++ show t ++ " and " ++ show u)

-- | Print an error message for failed expectation.
expected :: Show t => String -> Type t -> TypeM a
expected s t = throwError ("expected " ++ s ++ ", got: " ++ show t)

-- | Typing relation. Returns the type schema of a term and the updated
--   environment or else reports a type error.
typeOf :: Expr Refined -> TypeM (Schema Refined)
    -- environment lookup
typeOf (Ref x)      = lookRef x
typeOf (Use x)      = lookUse x
    -- introduction forms
-- typeOf (Abs x t b)  = typeWith x t (fmap (t :->) (typeOf b))
-- typeOf (Free e)     = liftM Bang (typeOf e)
