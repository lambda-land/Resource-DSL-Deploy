module DSL.Type where

import GHC.Generics (Generic)

import DSL.Environment
import DSL.Predicate
import DSL.Primitive


--
-- * Types and Schemas
--

-- | Type schemas.
data Schema t = Forall [Var] (Type t)
  deriving (Eq,Generic,Show)

-- | Optionally refined primitive types.
data Refine
     = Simple  PType
     | Refined PType Var Pred
  deriving (Eq,Generic,Show)

-- | Expression types.
data Type t
     = Base t              -- ^ base (simple or refined) type
     | Type t :-> Type t   -- ^ function type
     | Type t :*: Type t   -- ^ product type
  deriving (Eq,Generic,Show)

-- | The primitive type associated with a refinement.
ptype :: Refine -> PType
ptype (Simple t)      = t
ptype (Refined t _ _) = t

-- | Compose two refinements by combining the corresponding predicates with
--   the given function. Returns Nothing if the primitive types differ.
composeRefine :: (Pred -> Pred -> Pred) -> Refine -> Refine -> Maybe Refine
composeRefine _   (Simple t)      r@(Simple u)      | t == u = Just r
composeRefine _   (Simple t)      r@(Refined u v p) | t == u = Just r
composeRefine _ r@(Refined t v p)   (Simple u)      | t == u = Just r
composeRefine f   (Refined t v p)   (Refined u w q) | t == u = Just (Refined u w p')
  where p' | v == w    = f p q
           | otherwise = renameVar v w (f p q)
composeRefine _ _ _ = Nothing
