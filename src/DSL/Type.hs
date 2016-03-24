{-# LANGUAGE
      DeriveGeneric,
      GeneralizedNewtypeDeriving
  #-}

module DSL.Type where

import Data.String (IsString)
import GHC.Generics (Generic)


--
-- * Names
--

-- | Variable names.
newtype Var = Var String
  deriving (Eq,Generic,IsString,Ord,Show)

-- | Row variable names.
newtype RVar = RVar String
  deriving (Eq,Generic,IsString,Ord,Show)

-- | Record labels.
type Label = String


--
-- * Types and Schemas
--

-- | Rows.
type Row a = [(Label, a)]

-- | Type schemas.
data Schema a = Forall [RVar] (Type a)
  deriving (Eq,Generic,Show)

-- | Monomorphic type schema.
mono :: Type a -> Schema a
mono = Forall []

-- | Types.
data Type a
     = Base a
     | (Maybe Var, Type a) :-> Type a
     | TRec (Row (Type a)) (Maybe RVar)
  deriving (Eq,Generic,Show)

infixr 1 :->

-- | Basic types.
data Basic = TBool | TInt | TUnit
  deriving (Eq,Generic,Show)


--
-- * Liquid Types
--

-- | Liquid types.
type Liquid = (Basic,Qual)

-- | Logical qualifiers.
data Qual
     = QThis
     | QB Bool
     | QI Int
     | QRef Var
     | QNot Qual
     | QAnd Qual Qual
     | QOr  Qual Qual
     | QLte Qual Qual
     | QEqu Qual Qual
     | QAdd Qual Qual
  deriving (Eq,Generic,Show)

-- | Unconstrained basic type.
basic :: Basic -> Type Liquid
basic t = Base (t, QB True)

-- | Unconstrained boolean type.
tBool :: Type Liquid
tBool = basic TBool

-- | Unconstrained integer type.
tInt :: Type Liquid
tInt = basic TInt

-- | Unconstrained unit type.
tUnit :: Type Liquid
tUnit = basic TUnit

-- | Lookup the type associated with a label in a record type.
selectT :: Label -> Type a -> Maybe (Type a)
selectT l (TRec r _) = lookup l r
selectT _ _          = Nothing
