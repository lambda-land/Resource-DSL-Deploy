{-# LANGUAGE OverloadedStrings #-}

module DSL.Serialize where

import Control.Monad (liftM2,liftM3)

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Either (either)
import Data.Map.Strict (toAscList)
import Data.Scientific (toBoundedInteger)
import Data.Text (pack,unpack)
import Data.Vector (fromList,toList)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import qualified Data.ByteString.Lazy.Char8 as B

import DSL.Effect
import DSL.Environment
import DSL.Expression
import DSL.Model
import DSL.Parser
import DSL.Pretty
import DSL.Primitive
import DSL.Profile
import DSL.Resource


--
-- * Default File Paths
--

defaultDict   = "inbox/dictionary.json"
defaultInit   = "inbox/resources.json"
defaultModel  = "inbox/model.json"
defaultConfig = "inbox/configuration.json"
defaultReqs   = "inbox/requirements.json"
defaultOutput = "outbox/resources.json"


--
-- * Read/Write JSON Files
-- 

-- | Parse a String containing a JSON value.
decodeJSON :: FromJSON a => String -> IO a
decodeJSON = either fail return . eitherDecode . B.pack

-- | Read value from a JSON file.
readJSON :: FromJSON a => FilePath -> IO a
readJSON file = do
    mx <- fmap decode (B.readFile file)
    case mx of
      Just x  -> return x
      Nothing -> fail ("Error decoding JSON file: " ++ file)

-- | Write value to a JSON file, creating the directory if needed.
writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON file x = do
    createDirectoryIfMissing True (takeDirectory file)
    B.writeFile file (encodePretty x)


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

instance ToJSON PVal where
  toJSON Unit  = Null
  toJSON (B b) = Bool b
  toJSON (I i) = Number (fromInteger (toInteger i))

instance FromJSON PVal where
  parseJSON Null       = pure Unit
  parseJSON (Bool b)   = pure (B b)
  parseJSON (Number n) | Just i <- toBoundedInteger n = pure (I i)
  parseJSON bad = typeMismatch "PVal" bad


-- ** Functions and Expressions

instance ToJSON Param where
  toJSON (P pname ptype) = object
    [ "name" .= String (pack pname)
    , "type" .= toJSON ptype ]

instance FromJSON Param where
  parseJSON (Object o) = do
    pname <- o .: "name" 
    ptype <- o .: "type"
    return (P pname ptype)
  parseJSON bad = typeMismatch "Param" bad

instance ToJSON Fun where
  toJSON (Fun param body) = object
    [ "parameter" .= toJSON param
    , "body"      .= toJSON body ]

instance FromJSON Fun where
  parseJSON (Object o) = do
    param <- o .: "parameter"
    body  <- o .: "body"
    return (Fun param body)
  parseJSON bad = typeMismatch "Fun" bad

instance ToJSON Expr where
  toJSON = String . pack . pExpr

instance FromJSON Expr where
  parseJSON (String t) = either fail pure (parseExprText t)
  parseJSON bad = typeMismatch "Expr" bad


-- ** Environments

instance (ToJSON k, ToJSON v) => ToJSON (Env k v) where
  toJSON (Env m) = Array (fromList (map entry (toAscList m)))
    where
      entry (k,v) = object [ "key" .= toJSON k, "value" .= toJSON v ]

instance (FromJSON k, FromJSON v, Ord k, MergeDup v) => FromJSON (Env k v) where
  parseJSON (Array kvs) = fmap envFromListAcc (mapM entry (toList kvs))
    where
      entry (Object o) = do
        key <- o .: "key"
        val <- o .: "value"
        return (key,val)
  parseJSON bad = typeMismatch "Env" bad

instance ToJSON Entry where
  toJSON (ProEntry p) = object
    [ "type"  .= String "profile"
    , "entry" .= toJSON p ]
  toJSON (ModEntry m) = object
    [ "type"  .= String "model"
    , "entry" .= toJSON m ]

instance FromJSON Entry where
  parseJSON (Object o) = do
    t <- o .: "type"
    e <- o .: "entry"
    case t of
      String "profile" -> fmap ProEntry (parseJSON e)
      String "model"   -> fmap ModEntry (parseJSON e)
      _ -> fail $ "invalid entry type: " ++ show t
  parseJSON bad = typeMismatch "Entry" bad


-- ** Effects and Profiles

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

instance FromJSON Effect where
  parseJSON (Object o) = do
    eff <- o .: "effect"
    case eff of
      String "create" -> fmap Create (o .: "expression" >>= parseJSON)
      String "check"  -> fmap Check  (o .: "function" >>= parseJSON)
      String "modify" -> fmap Modify (o .: "function" >>= parseJSON)
      String "delete" -> return Delete
      _ -> fail $ "invalid effect name: " ++ show eff
  parseJSON bad = typeMismatch "Effect" bad

instance ToJSON Profile where
  toJSON (Profile xs effs) = object
    [ "parameters" .= toJSON xs
    , "effects"    .= toJSON effs ]

instance FromJSON Profile where
  parseJSON (Object o) = do
    xs   <- o .: "parameters"
    effs <- o .: "effects"
    return (Profile xs effs)
  parseJSON bad = typeMismatch "Profile" bad


-- ** Statements and Models

instance ToJSON Stmt where
  toJSON (Do name eff) = object
    [ "statement" .= String "do"
    , "name"      .= String (pack name)
    , "effect"    .= toJSON eff ]
  toJSON (In ctx body) = object
    [ "statement" .= String "in"
    , "context"   .= toJSON ctx
    , "body"      .= toJSON body ]
  toJSON (If cond tru fls) = object
    [ "statement" .= String "if"
    , "condition" .= toJSON cond
    , "then"      .= toJSON tru
    , "else"      .= toJSON fls ]
  toJSON (Let x bound body) = object
    [ "statement" .= String "let"
    , "variable"  .= String (pack x)
    , "bound"     .= toJSON bound
    , "body"      .= toJSON body ]
  toJSON (Load name args) = object
    [ "statement" .= String "load"
    , "name"      .= String (pack name)
    , "arguments" .= toJSON args ]

instance FromJSON Stmt where
  parseJSON (Object o) = do
      stmt <- o .: "statement"
      case stmt of
        String "do"   -> liftM2 Do (string "name") (object "effect")
        String "in"   -> liftM2 In (object "context") (object "body")
        String "if"   -> liftM3 If (object "condition") (object "then") (object "else")
        String "let"  -> liftM3 Let (string "variable") (object "bound") (object "body")
        String "load" -> liftM2 Load (string "name") (object "arguments")
        _ -> fail $ "invalid statement type: " ++ show stmt
    where
      string k = do String t <- o .: k; return (unpack t)
      object k = o .: k >>= parseJSON
  parseJSON bad = typeMismatch "Stmt" bad

instance ToJSON Model where
  toJSON (Model xs block) = object
    [ "parameters" .= toJSON xs
    , "block"      .= toJSON block ]

instance FromJSON Model where
  parseJSON (Object o) = do
    xs    <- o .: "parameters"
    block <- o .: "block"
    return (Model xs block)
  parseJSON bad = typeMismatch "Model" bad
