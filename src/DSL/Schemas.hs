{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DSL.Schemas where

-- import DSL.Effect
-- import DSL.Environment
import DSL.Expression
-- import DSL.Model
-- import DSL.Parser
-- import DSL.Pretty
import DSL.Primitive
-- import DSL.Profile
-- import DSL.Resource

import DSL.Serialize
import qualified Data.Aeson             as A
import           Data.JSON.Schema
import           Data.JSON.Schema.Validate

-- Primitives
pTypeSchema:: Schema
pTypeSchema = Choice [Constant A.Null, Boolean, Number unbounded, Value unboundedLength]
