{-# LANGUAGE
      DeriveGeneric,
      FlexibleContexts
  #-}

module DSL.Type where

import GHC.Generics (Generic)

import Control.Monad.Except (MonadError,throwError)

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

data TypeError
     = PTypeMisMatch
     | DeleteUse
     | UseCreate
  deriving (Eq,Generic,Show)

-- | The primitive type associated with a refinement.
ptype :: Refinement -> PType
ptype (Simple t)      = t
ptype (Refined t _ _) = t

-- | Compose two refinements.
composeRefinement :: MonadError TypeError m => Refinement -> Refinement -> m Refinement
composeRefinement   (Simple t)      r@(Simple u)      | t == u = return r
composeRefinement   (Simple t)      r@(Refined u v p) | t == u = return r
composeRefinement r@(Refined t v p)   (Simple u)      | t == u = return r
composeRefinement   (Refined t v p)   (Refined u w q) | t == u = return (Refined u w p')
  where p' | v == w    = p &&& q
           | otherwise = renameVar v w p &&& q
composeRefinement r1 r2 = throwError PTypeMisMatch

-- | Compose two resource environment actions.
composeAction :: MonadError TypeError m => Action -> Action -> m Action

composeAction (Check r1)     (Check r2)     = fmap Check (composeRefinement r1 r2)
composeAction (Check r1)     (Delete r2)    = fmap Delete (composeRefinement r1 r2)
composeAction (Check r1)     (Modify r2 r3) = fmap (flip Modify r3) (composeRefinement r1 r2)

composeAction (Create r1)    (Check r2)     = fmap Create (composeRefinement r1 r2)
composeAction (Create r1)    (Delete r2)    = undefined -- check r1 r2
composeAction (Create r1)    (Modify r2 r3) = undefined -- check r2 r2 >> Create r3

composeAction (Delete r1)    (Create r2)    = return (Modify r1 r2)

composeAction (Modify r1 r2) (Check r3)     = fmap (Modify r1) (composeRefinement r2 r3)
composeAction (Modify r1 r2) (Delete r3)    = undefined -- check r2 r3 >> Delete r1
composeAction (Modify r1 r2) (Modify r3 r4) = undefined -- check r2 r3 >> Modify r1 r4

composeAction (Delete _)     _              = throwError DeleteUse
composeAction _              (Create _)     = throwError UseCreate
