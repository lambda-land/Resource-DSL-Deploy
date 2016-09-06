{-# LANGUAGE OverloadedStrings #-}

module DSL.Serialize where

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Encode.Pretty (encodePretty)

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Text (pack,unpack)

import System.Directory (createDirectoryIfMissing)

import DSL.Effect
import DSL.Environment
import DSL.Expression
import DSL.Pretty
import DSL.Primitive


--
-- * Read/Write JSON Files
-- 

-- | Construct a JSON file name from a directory and file name.
jsonFile :: FilePath -> FilePath -> FilePath
jsonFile dir file = dir ++ "/" ++ file ++ ".json"

-- | Write a JSON file to the given directory with the given name,
--   creating the directory if needed.
writeJSON :: ToJSON a => FilePath -> FilePath -> a -> IO ()
writeJSON dir file x = do
    createDirectoryIfMissing True dir
    B.writeFile (jsonFile dir file) (encodePretty x)

-- | Read a JSON file from the given directory with the given name.
readJSON :: FromJSON a => FilePath -> FilePath -> IO a
readJSON dir file = do
    mx <- fmap decode (B.readFile (jsonFile dir file))
    case mx of
      Just x  -> return x
      Nothing -> fail ("Error decoding JSON file: " ++ jsonFile dir file)


--
-- * Instances
--

-- ** Primitives

instance ToJSON PType where
  toJSON TUnit = String "unit"
  toJSON TBool = String "bool"
  toJSON TInt  = String "int"

instance FromJSON PType where
  parseJSON (String "unit") = pure TUnit
  parseJSON (String "bool") = pure TBool
  parseJSON (String "int")  = pure TInt
  parseJSON bad = typeMismatch "PType" bad


-- ** Functions and Expressions

instance ToJSON Param where
  toJSON (P pname ptype) = object
    [ "name" .= String (pack pname)
    , "type" .= String (pack (show ptype)) ]

instance FromJSON Param where
  parseJSON (Object o) = do
    pname <- o .: "name" 
    ptype <- o .: "type"
    return (P pname ptype)
  parseJSON bad = typeMismatch "Param" bad

instance ToJSON Fun where
  toJSON (Fun param body) = object
    [ "param" .= toJSON param
    , "body"  .= toJSON body ]

instance FromJSON Fun where
  parseJSON (Object o) = do
    param <- o .: "param"
    body  <- o .: "body"
    return (Fun param body)
  parseJSON bad = typeMismatch "Fun" bad

instance ToJSON Expr where
  toJSON = String . pack . pExpr

instance FromJSON Expr where
  parseJSON = undefined


-- ** Effects

instance ToJSON Effect where
  toJSON (Create expr) = object
    [ "effect"     .= String "create"
    , "expression" .= toJSON expr ]
  toJSON (Check fun) = object
    [ "effect"     .= String "check"
    , "function"   .= toJSON fun ]
  toJSON (Modify fun) = object
    [ "effect"     .= String "modify"
    , "function"   .= toJSON fun ]
  toJSON Delete = object
    [ "effect"     .= String "delete" ]
