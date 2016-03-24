{-# LANGUAGE DeriveGeneric #-}

module DSL.Type where

import GHC.Generics (Generic)


-- | Variable names.
type Var = String

-- | Record labels.
type Label = String

-- | Type schemas.
data Schema a = Forall [Var] (Type a)
  deriving (Eq,Generic,Show)

-- | Monomorphic type schema.
mono :: Type a -> Schema a
mono = Forall []

-- | Types.
data Type a
     = Base a
     | (Var, Type a) :-> Type a
     | Rec [(Label, Type a)] (Maybe Var)
  deriving (Eq,Generic,Show)

infixr 1 :->

-- | Basic types.
data Basic = TBool | TInt | TUnit
  deriving (Eq,Generic,Show)

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
