{-# LANGUAGE DeriveGeneric #-}

module DSL.Type where

import GHC.Generics (Generic)

import DSL.Environment
import DSL.Predicate
import DSL.Primitive


--
-- * Types and Schemas
--

-- | Type schemas.
-- data Schema = Forall [Var] (Type t)
  -- deriving (Eq,Generic,Show)

-- | Optionally refined primitive types.
data Refinement
     = Simple  PType
     | Refined PType Var Pred
  deriving (Eq,Generic,Show)

-- | Expression types.
data Type t
     = Base t              -- ^ base (simple or refined) type
     | Type t :-> Type t   -- ^ function type
     | Type t :*: Type t   -- ^ product type
  deriving (Eq,Generic,Show)

-- | An action to perform on the resource environment.
data Action
     = Check  Refinement
     | Create Refinement
     | Delete Refinement
     | Modify Refinement Refinement
  deriving (Eq,Generic,Show)

-- | Resource type.
data RType = RT [(Var,PType)] (HEnv Action)
  deriving (Eq,Generic,Show) 
