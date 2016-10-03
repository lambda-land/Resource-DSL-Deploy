module DSL.Serialize where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Data.Aeson
import Data.Aeson.Types hiding (parse)
import Data.Aeson.BetterErrors
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Map.Strict (toAscList)
import Data.Monoid ((<>))
import Data.String (fromString)
import Data.Text (Text,intercalate,pack,unpack)
import Data.Vector (fromList)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.Exit

import qualified Data.ByteString.Lazy.Char8 as B

import DSL.Effect
import DSL.Environment
import DSL.Expression
import DSL.Model
import DSL.Name
import DSL.Parser
import DSL.Path
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
-- * Read/Write JSON
-- 

-- | Parse a String containing a JSON value.
decodeJSON :: String -> ParseIt a -> IO a
decodeJSON s p = handleParseError Nothing (parse p (B.pack s))

-- | Read value from a JSON file.
readJSON :: FilePath -> ParseIt a -> IO a
readJSON file p = do
    content <- B.readFile file
    handleParseError (Just file) (parse p content)

-- | Write value to a JSON file, creating the directory if needed.
writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON file x = do
    createDirectoryIfMissing True (takeDirectory file)
    B.writeFile file (encodePretty x)

-- | Convert the result of a JSON parser to an IO action. Either print the
--   error and exit or return the result.
handleParseError :: Maybe FilePath -> Either (ParseError SchemaViolation) a -> IO a
handleParseError _   (Right ok) = return ok
handleParseError loc (Left err) = do
    putStrLn ("Error parsing JSON " ++ maybe "string" ("file: " ++) loc)
    printParseError err
    exitWith (ExitFailure 1)


--
-- * Conversion
--

-- ** Errors

type ParseIt a = Parse SchemaViolation a

data SchemaViolation
     = BadCase Text [Text] Text
     | BadPVal Value
     | ExprParseError String Text
  deriving (Data,Eq,Generic,Read,Show,Typeable)

prettySchemaViolation :: SchemaViolation -> Text
prettySchemaViolation (BadCase typ good bad) =
    "Invalid " <> typ <> " case: " <> bad 
    <> "\nExpected one of: " <> intercalate ", " good
prettySchemaViolation (BadPVal bad) =
    "Invalid primitive value: " <> pack (show bad)
prettySchemaViolation (ExprParseError msg bad) =
    "Error parsing expression: " <> bad
    <> "\n" <> pack msg

printParseError :: ParseError SchemaViolation -> IO ()
printParseError = mapM_ (putStrLn . unpack) . displayError prettySchemaViolation


-- ** Primitives

instance ToJSON PType where
  toJSON TUnit   = String "unit"
  toJSON TBool   = String "bool"
  toJSON TInt    = String "int"
  toJSON TSymbol = String "symbol"

instance ToJSON PVal where
  toJSON Unit  = Null
  toJSON (B b) = Bool b
  toJSON (I i) = Number (fromInteger (toInteger i))
  toJSON (S s) = String (pack (toName s))

asPType :: ParseIt PType
asPType = do
    t <- asText
    case t of
      "unit"   -> pure TUnit
      "bool"   -> pure TBool
      "int"    -> pure TInt
      "symbol" -> pure TSymbol
      _ -> throwCustomError (BadCase "primitive type" ["unit","bool","int","symbol"] t)

asPVal :: ParseIt PVal
asPVal = do
    v <- asValue
    case v of
      Null     -> pure Unit
      Bool b   -> pure (B b)
      Number n -> I <$> asIntegral
      String n -> S <$> asSymbol
      _ -> throwCustomError (BadPVal v)

asConfig :: ParseIt [PVal]
asConfig = eachInArray asPVal
     

-- ** Functions and Expressions

instance ToJSON Param where
  toJSON (Param pname ptype) = object
    [ "name" .= String (pack pname)
    , "type" .= toJSON ptype ]

instance ToJSON Fun where
  toJSON (Fun param body) = object
    [ "parameter" .= toJSON param
    , "body"      .= toJSON body ]

instance ToJSON Expr where
  toJSON = String . pack . prettyExpr

asParam :: ParseIt Param
asParam = Param <$> key "name" asName <*> key "type" asPType

asFun :: ParseIt Fun
asFun = Fun <$> key "parameter" asParam <*> key "body" asExpr

asExpr :: ParseIt Expr
asExpr = do
    t <- asText
    case parseExprText t of
      Right e  -> pure e
      Left msg -> throwCustomError (ExprParseError msg t)


-- ** Names and Paths

instance ToJSON Symbol where
  toJSON (Symbol n) = toJSON n

instance ToJSON ResID where
  toJSON (ResID p) = toJSON p

instance ToJSON Path where
  toJSON = String . pack . prettyPath

asName :: ParseIt Name
asName = asString

asSymbol :: ParseIt Symbol
asSymbol = mkSymbol <$> asName

asResID :: ParseIt ResID
asResID = ResID <$> eachInArray asName

asPath :: ParseIt Path
asPath = fromString <$> asString


-- ** Environments

instance (ToJSON k, ToJSON v) => ToJSON (Env k v) where
  toJSON (Env m) = Array (fromList (map entry (toAscList m)))
    where entry (k,v) = object [ "key" .= toJSON k, "value" .= toJSON v ]

instance ToJSON Entry where
  toJSON (ProEntry p) = object
    [ "type"  .= String "profile"
    , "entry" .= toJSON p ]
  toJSON (ModEntry m) = object
    [ "type"  .= String "model"
    , "entry" .= toJSON m ]

asEnv :: (Ord k, MergeDup v) => ParseIt k -> ParseIt v -> ParseIt (Env k v)
asEnv asKey asVal = envFromListAcc <$> eachInArray entry
  where entry = (,) <$> key "key" asKey <*> key "value" asVal

asEntry :: ParseIt Entry
asEntry = do
    t <- key "type" asText
    case t of
      "profile" -> ProEntry <$> key "entry" asProfile
      "model"   -> ModEntry <$> key "entry" asModel
      _ -> throwCustomError (BadCase "dictionary entry" ["profile","model"] t)

asVarEnv :: ParseIt VarEnv
asVarEnv = asEnv asName asPVal

asResEnv :: ParseIt ResEnv
asResEnv = asEnv asResID asPVal

asDictionary :: ParseIt Dictionary
asDictionary = asEnv asSymbol asEntry


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

instance ToJSON Profile where
  toJSON (Profile xs effs) = object
    [ "parameters" .= toJSON xs
    , "effects"    .= toJSON effs ]

asEffect :: ParseIt Effect
asEffect = do
    eff <- key "effect" asText
    case eff of
      "create" -> Create <$> key "expression" asExpr
      "check"  -> Check  <$> key "function" asFun
      "modify" -> Modify <$> key "function" asFun
      "delete" -> pure Delete
      _ -> throwCustomError (BadCase "effect" ["create","check","modify","delete"] eff)

asProfile :: ParseIt Profile
asProfile = Profile
    <$> key "parameters" (eachInArray asParam)
    <*> key "effects" (asEnv asPath (eachInArray asEffect))


-- ** Statements and Models

instance ToJSON Stmt where
  toJSON (Do path eff) = object
    [ "statement" .= String "do"
    , "path"      .= toJSON path
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
  toJSON (Load comp args) = object
    [ "statement" .= String "load"
    , "component" .= toJSON comp
    , "arguments" .= toJSON args ]

instance ToJSON Model where
  toJSON (Model xs block) = object
    [ "parameters" .= toJSON xs
    , "block"      .= toJSON block ]

asBlock :: ParseIt Block
asBlock = eachInArray asStmt

asStmt :: ParseIt Stmt
asStmt = do
    stmt <- key "statement" asText
    case stmt of
      "do"   -> Do   <$> key "path"      asPath
                     <*> key "effect"    asEffect
      "in"   -> In   <$> key "context"   asPath
                     <*> key "body"      asBlock
      "if"   -> If   <$> key "condition" asExpr
                     <*> key "then"      asBlock
                     <*> key "else"      asBlock
      "let"  -> Let  <$> key "variable"  asName
                     <*> key "bound"     asExpr
                     <*> key "body"      asBlock
      "load" -> Load <$> key "component" asExpr
                     <*> key "arguments" (eachInArray asExpr)
      _ -> throwCustomError (BadCase "stmt" ["do","in","if","let","load"] stmt)

asModel :: ParseIt Model
asModel = Model
    <$> key "parameters" (eachInArray asParam)
    <*> key "block" asBlock
