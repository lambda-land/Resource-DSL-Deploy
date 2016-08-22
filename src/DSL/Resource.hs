{-# LANGUAGE
      DeriveGeneric,
      FlexibleContexts
  #-}

module DSL.Resource where

import GHC.Generics (Generic)

import Control.Monad.Except (MonadError,throwError)
import Control.Monad.Writer (MonadWriter,tell)

import DSL.Environment
import DSL.Predicate
import DSL.Primitive
import DSL.Type

--
-- * Resource Effects
--

-- | An effect on a particular resource.
data Effect
     = Noop
     | Check  Refine
     | Create Refine
     | Delete Refine
     | Modify Refine Refine
  deriving (Eq,Generic,Show)

-- | An error produced by composing resource effects.
data EffectError
     = PTypeMisMatch
     | DeleteUse
     | UseCreate
  deriving (Eq,Generic,Show)

-- | Compose two refinements, raising a PTypeMisMatch effect error if the
--   primitive types don't agree.
composeRefineEffect :: MonadError EffectError m
  => (Pred -> Pred -> Pred) -> Refine -> Refine -> m Refine
composeRefineEffect p r1 r2 = case composeRefine p r1 r2 of
    Just r  -> return r
    Nothing -> throwError PTypeMisMatch

-- | Conjunction of two refinements.
checkBoth :: MonadError EffectError m => Refine -> Refine -> m Refine
checkBoth = composeRefineEffect (&&&)

-- | Check that an output refinement is compatible with the next input refinement.
checkSide :: (MonadError EffectError m, MonadWriter [Refine] m)
  => Refine -> Refine -> m ()
checkSide r1 r2 = do
    r <- composeRefineEffect (==>) r1 r2
    tell [r]

-- | Compose two resource effects. The Writer monad tracks side conditions
--   that must be checked in addition to the refinements in the resulting
--   effect.
composeEffect :: (MonadError EffectError m, MonadWriter [Refine] m)
  => Effect -> Effect -> m Effect
composeEffect Noop           e              = return e
composeEffect e              Noop           = return e
composeEffect (Check r1)     (Check r2)     = fmap Check (checkBoth r1 r2)
composeEffect (Check r1)     (Delete r2)    = fmap Delete (checkBoth r1 r2)
composeEffect (Check r1)     (Modify r2 r3) = fmap (flip Modify r3) (checkBoth r1 r2)
composeEffect (Create r1)    (Check r2)     = fmap Create (checkBoth r1 r2)
composeEffect (Create r1)    (Delete r2)    = checkSide r1 r2 >> return Noop
composeEffect (Create r1)    (Modify r2 r3) = checkSide r1 r2 >> return (Create r3)
composeEffect (Delete r1)    (Create r2)    = return (Modify r1 r2)
composeEffect (Modify r1 r2) (Check r3)     = fmap (Modify r1) (checkBoth r2 r3)
composeEffect (Modify r1 r2) (Delete r3)    = checkSide r2 r3 >> return (Delete r1)
composeEffect (Modify r1 r2) (Modify r3 r4) = checkSide r2 r3 >> return (Modify r1 r4)
composeEffect (Delete _)     _              = throwError DeleteUse
composeEffect _              (Create _)     = throwError UseCreate


--
-- * Resource Types
--

-- | A named parameter of primitive type.
type Param = (Var,PType)

-- | Resource type.
data RType = RT {
    rtParams :: [Param],     -- ^ parameters
    rtEffect :: HEnv Effect  -- ^ effects on resources
} deriving (Eq,Generic,Show) 

-- | Compose two resource types.
composeRType :: (MonadError EffectError m, MonadWriter [Refine] m)
  => (Var -> Var) -> RType -> RType -> m RType
composeRType = undefined
