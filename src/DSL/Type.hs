{-# LANGUAGE DeriveGeneric #-}

module DSL.Type where

import GHC.Generics (Generic)

import DSL.Env
import DSL.Predicate
import DSL.Row


--
-- * Types and Schemas
--

-- | Type schemas.
data Schema t = Forall [Var] (Type t)
  deriving (Eq,Generic,Show)

-- | Types.
data Type t
     = Base t                           -- ^ base (simple or refined) type
     | Bang (Type t)                    -- ^ reusable type (linear logic: "of course")
     | Type t :-> Type t                -- ^ function type
     | TRec (Row (Type t)) (Maybe Var)  -- ^ record type (row type + optional row variable)
  deriving (Eq,Generic,Show)

infixr 1 :->

-- | Simple base types.
data Simple = TBool | TInt | TUnit
  deriving (Eq,Generic,Show)

-- | Monomorphic type schema.
mono :: Type t -> Schema t
mono = Forall []

-- | Smart constructor for monomorphic record types.
monoRec :: [(Label, Type t)] -> Type t
monoRec r = TRec (row r) Nothing

-- | Smart constructor for row-polymorphic record types.
polyRec :: Var -> [(Label, Type t)] -> Type t
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
selectT :: Label -> Type t -> Maybe (Type t)
selectT l (TRec r _) = rowLookup l r
selectT _ _          = Nothing
