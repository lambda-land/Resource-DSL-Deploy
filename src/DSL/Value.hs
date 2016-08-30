module DSL.Value where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Control.Monad       (liftM2)
import Control.Monad.Catch (MonadThrow)

import DSL.Predicate
import DSL.Primitive


--
-- * Values
--

-- | Variational primitive values.
data Value
     = Prim PVal               -- ^ basic primitive value
     | ChcV BExpr Value Value  -- ^ choice value
  deriving (Data,Eq,Generic,Read,Show,Typeable)

-- | Check whether a predicate is true for all leaves of a value.
--   A result of Nothing indicates a type error.
allPrims :: (PVal -> Maybe Bool) -> Value -> Maybe Bool
allPrims f (Prim v)     = f v
allPrims f (ChcV _ l r) = liftM2 (&&) (allPrims f l) (allPrims f r)

-- | Check whether a value is equivalent to True.
--   A result of Nothing indicates a type error.
valIsTrue :: Value -> Maybe Bool
valIsTrue = allPrims isTrue
  where isTrue (B b) = Just b
        isTrue _     = Nothing

-- | Apply a primitive unary function to a value.
applyPrim1 :: MonadThrow m => Op1 -> Value -> m Value
applyPrim1 o (Prim v)     = fmap Prim (primOp1 o v)
applyPrim1 o (ChcV d l r) = liftM2 (ChcV d) (applyPrim1 o l) (applyPrim1 o r)

-- | Apply a primitive binary function to two values.
applyPrim2 :: MonadThrow m => Op2 -> Value -> Value -> m Value
applyPrim2 o (Prim l) (Prim r) = fmap Prim (primOp2 o l r)
applyPrim2 o (ChcV d ll lr) r  = liftM2 (ChcV d) (applyPrim2 o ll r) (applyPrim2 o lr r)
applyPrim2 o l (ChcV d rl rr)  = liftM2 (ChcV d) (applyPrim2 o l rl) (applyPrim2 o l rr)
