{-# LANGUAGE OverloadedStrings #-}

module DSL.Schemas where

import DSL.Effect
import DSL.Environment
import DSL.Expression
import DSL.Model
import DSL.Parser
import DSL.Pretty
import DSL.Primitive
import DSL.Profile
import DSL.Resource

import Data.JSON.Schema

-- Primitives
instance JSONSchema PType where
  schema = gSchema

instance JSONSchema PVal where
  schema = gSchema

instance JSONSchema Param where
  schema (P pname ptype) = Object
    [ Field { key = "name", required = True, content = schema $ String (pack pname)}
    , Field { key = "type", required = True, content = schema ptype}
    ]
