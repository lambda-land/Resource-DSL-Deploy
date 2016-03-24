{-# LANGUAGE DeriveGeneric #-}

module DSL.Type where

import Data.String (IsString)
import GHC.Generics (Generic)


--
-- * Names
--

-- | Variable names.
type Var = String

-- | Record labels.
type Label = String


--
-- * Types and Schemas
--

-- | Rows.
type Row a = [(Label, a)]

-- | Type schemas.
data Schema a = Forall [Var] (Type a)
  deriving (Eq,Generic,Show)

-- | Monomorphic type schema.
mono :: Type a -> Schema a
mono = Forall []

-- | Types.
data Type a
     = Base a
     | Type a :-> Type a
     | TRec (Row (Type a)) (Maybe Var)
  deriving (Eq,Generic,Show)

infixr 1 :->

-- | Basic types.
data Basic = TBool | TInt | TUnit
  deriving (Eq,Generic,Show)


--
-- * Refinement Types
--

-- | Refinement types.
type Refined = (Basic,Pred)

-- | Predicates.
data Pred
     = PThis
     | PB Bool
     | PI Int
     | PRef Var
     | PNot Pred
     | PAnd Pred Pred
     | POr  Pred Pred
     | PLte Pred Pred
     | PEqu Pred Pred
     | PAdd Pred Pred
  deriving (Eq,Generic,Show)

-- | Unconstrained basic type.
basic :: Basic -> Type Refined
basic t = Base (t, PB True)

-- | Unconstrained boolean type.
tBool :: Type Refined
tBool = basic TBool

-- | Unconstrained integer type.
tInt :: Type Refined
tInt = basic TInt

-- | Unconstrained unit type.
tUnit :: Type Refined
tUnit = basic TUnit

-- | Lookup the type associated with a label in a record type.
selectT :: Label -> Type a -> Maybe (Type a)
selectT l (TRec r _) = lookup l r
selectT _ _          = Nothing
