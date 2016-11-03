{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import           Data.JSON.Schema
import           Data.Proxy

-- Primitives
instance JSONSchema PVal where
-- schema (Proxy :: Proxy Unit) = Constant A.Null -- stuck on Proxies again
--   schema (TBool) = Boolean
--   schema TInt  = Number unbounded

schemaOf (v :: x) = schema (Proxy :: Proxy x)
