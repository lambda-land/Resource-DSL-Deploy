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

import           Data.Aeson                (Value (..), decode)
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.List.NonEmpty     as NE
import           Data.Maybe                (fromMaybe)
import qualified Data.JsonSchema.Draft4 as D4

-- Primitives

-- primitives :: IO ()
primitives = do
  -- Get Primitives Schemas
  prims <- LBS.readFile "./json/definitions.json"
  let schema = fromMaybe (error "Invalid schema JSON.") (decode prims)
      schemaWithURI = D4.SchemaWithURI schema (Just "./json/configuration-schema.json")
  return schemaWithURI
