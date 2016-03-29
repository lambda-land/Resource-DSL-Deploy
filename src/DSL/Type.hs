{-# LANGUAGE DeriveGeneric #-}

module DSL.Type where

import GHC.Generics (Generic)

import Data.Map (Map)
import qualified Data.Map as Map

import DSL.Env
import DSL.Predicate


--
-- * Types and Schemas
--

-- | Record labels.
type Label = String

-- | Rows.
type Row a = Map Label a

-- | Type schemas.
data Schema a = Forall [Var] (Type a)
  deriving (Eq,Generic,Show)

-- | Types.
data Type a
     = Base a                           -- ^ base (simple or refined) type
     | Bang (Type a)                    -- ^ reusable type (linear logic: "of course")
     | Type a :-> Type a                -- ^ function type
     | TRec (Row (Type a)) (Maybe Var)  -- ^ record type (row type + optional row variable)
  deriving (Eq,Generic,Show)

infixr 1 :->

-- | Simple base types.
data Simple = TBool | TInt | TUnit
  deriving (Eq,Generic,Show)

-- | Monomorphic type schema.
mono :: Type a -> Schema a
mono = Forall []

-- | Smart constructor for rows.
row :: [(Label,a)] -> Row a
row = Map.fromList

-- | Smart constructor for monomorphic record types.
monoRec :: [(Label, Type a)] -> Type a
monoRec r = TRec (row r) Nothing

-- | Smart constructor for row-polymorphic record types.
polyRec :: Var -> [(Label, Type a)] -> Type a
polyRec v r = TRec (row r) (Just v)


--
-- * Refinement Types
--

-- | Refined base types.
data Refined
     = Simple Simple
     | Refined Simple Var BPred
  deriving (Eq,Generic,Show)

-- | Smart constructor for unconstrained simple types.
simple :: Simple -> Type Refined
simple = Base . Simple

-- | Smart constructor for refined types.
(??) :: (Var,Simple) -> BPred -> Type Refined
(??) (v,t) p = Base (Refined t v p)

infix 0 ??

-- | Unconstrained boolean type.
tBool :: Type Refined
tBool = simple TBool

-- | Unconstrained integer type.
tInt :: Type Refined
tInt = simple TInt

-- | Unconstrained unit type.
tUnit :: Type Refined
tUnit = simple TUnit

-- | Lookup the type associated with a label in a record type.
selectT :: Label -> Type a -> Maybe (Type a)
selectT l (TRec r _) = Map.lookup l r
selectT _ _          = Nothing
