{-# LANGUAGE DeriveGeneric #-}

module DSL.Type where

--
-- * Types and Schemas
--

-- | Type schemas.
data Schema t = Forall [Var] (Type t)
  deriving (Eq,Generic,Show)

-- | Types.
data Type t
     = Base t              -- ^ base (simple or refined) type
     | Type t :-> Type t   -- ^ function type
     | Type t :*: Type t   -- ^ product type
  deriving (Eq,Generic,Show)

