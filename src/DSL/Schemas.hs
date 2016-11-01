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

import qualified Data.Aeson             as A
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.List.NonEmpty     as NE
import           Data.Maybe                (fromMaybe)
import qualified Data.JsonSchema.Draft4 as D4
import           Data.JSON.Schema

-- Primitives

primitives :: D4.Schema
primitives = D4.emptySchema { D4._schemaRef = Just "./configuration-schema.json" }

schemaContext :: D4.SchemaWithURI D4.Schema
schemaContext = D4.SchemaWithURI
                { D4._swSchema = primitives
                , D4._swURI    = Just ".json/configuration-schema.json"
                }

instance JSONSchema PType where
  schema (TUnit) = Constant A.Null -- stuck on Proxies again
  schema (TBool) = Boolean
  schema TInt  = Number unbounded
