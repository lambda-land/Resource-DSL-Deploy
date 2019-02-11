module DSL.Serialize where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Data.Aeson
import Data.Aeson.BetterErrors
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Map.Strict (toAscList)
import Data.Scientific (floatingOrInteger,fromFloatDigits)
import Data.Text (Text,intercalate,pack,unpack)
import Data.Vector (fromList)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.Exit
import qualified Data.Set as S

import qualified Data.ByteString.Lazy.Char8 as B

import DSL.Types
import DSL.Environment
import DSL.Name
import DSL.Parser
import DSL.Path
import DSL.Pretty


--
-- * Default File Paths
--

defaultDict, defaultInit, defaultModel, defaultConfig, defaultReqs,
  defaultOutput, defaultError, defaultCtx, defaultBest :: FilePath
defaultDict   = "inbox/dictionary.json"
defaultInit   = "inbox/resources.json"
defaultModel  = "inbox/model.json"
defaultConfig = "inbox/configuration.json"
defaultReqs   = "inbox/requirements.json"
defaultOutput = "outbox/resources.json"
defaultError  = "outbox/error.json"
defaultCtx    = "outbox/success.json"
defaultBest   = "outbox/best.txt"

-- ** Errors

type ParseIt a = Parse SchemaViolation a

data SchemaViolation
     = BadCase Text [Text] Text
     | BadPVal Data.Aeson.Value
     | ExprParseError Text Text
     | BExprParseError Text Text
  deriving (Data,Eq,Generic,Read,Show,Typeable)

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

prettySchemaViolation :: SchemaViolation -> Text
prettySchemaViolation (BadCase typ good bad) =
    "Invalid " <> typ <> " case: " <> bad
    <> "\nExpected one of: " <> intercalate ", " good
prettySchemaViolation (BadPVal bad) =
    "Invalid primitive value: " <> pack (show bad)
prettySchemaViolation (ExprParseError msg bad) =
    "Error parsing expression: " <> bad
    <> "\n" <> msg
prettySchemaViolation (BExprParseError msg bad) =
    "Error parsing boolean expression: " <> bad
    <> "\n" <> msg

printParseError :: ParseError SchemaViolation -> IO ()
printParseError = mapM_ (putStrLn . unpack) . displayError prettySchemaViolation

asMaybe :: ParseIt a -> ParseIt (Maybe a)
asMaybe asA = nothing <|> just
  where
    nothing = Nothing <$ asNull
    just = Just <$> asA

asName :: ParseIt Name
asName = asText

instance ToJSON Symbol where
  toJSON (Symbol n) = toJSON n

asSymbol :: ParseIt Symbol
asSymbol = mkSymbol <$> asName

instance ToJSON Path where
  toJSON = String . prettyPath

asPath :: ParseIt Path
asPath = fromTextPath <$> asText

instance ToJSON ResID where
  toJSON (ResID p) = toJSON p

asResID :: ParseIt ResID
asResID = ResID <$> eachInArray asName

instance ToJSON BExpr where
  toJSON = String . pretty

asBExpr :: ParseIt BExpr
asBExpr = do
    t <- asText
    case parseBExprText t of
      Right e  -> pure e
      Left msg -> throwCustomError (BExprParseError (pack msg) t)

instance (ToJSON a) => ToJSON (V a) where
  toJSON (One a) = object ["vtype" .= String "one", "one" .= toJSON a]
  toJSON (Chc d l r) = object ["vtype" .= String "chc", "dim" .= d, "l" .= l, "r" .= r]

asV :: ParseIt a -> ParseIt (V a)
asV asA = do
  vtype <- key "vtype" asText
  case vtype of
    "one" -> one
    "chc" -> chc
    _ -> throwCustomError (BadCase "vtype" ["one", "chc"] vtype)
  where
    one = One <$> key "one" asA
    chc = Chc <$> key "dim" asBExpr <*> key "l" (asV asA) <*> key "r" (asV asA)

instance ToJSON PType where
  toJSON = String . pretty

asPType :: ParseIt PType
asPType = do
    t <- asText
    case t of
      "unit"   -> pure TUnit
      "bool"   -> pure TBool
      "int"    -> pure TInt
      "float"  -> pure TFloat
      "symbol" -> pure TSymbol
      _ -> throwCustomError (BadCase "primitive type" ["unit","bool","int","float","symbol"] t)

instance ToJSON PVal where
  toJSON Unit  = String "()"
  toJSON (B b) = Bool b
  toJSON (I i) = Number (fromInteger (toInteger i))
  toJSON (F d) = Number (fromFloatDigits d)
  toJSON (S s) = String (toName s)

asPVal :: ParseIt PVal
asPVal = do
    v <- asValue
    case v of
      Bool b   -> pure (B b)
      Number n -> pure (either F I (floatingOrInteger n))
      String "()" -> pure Unit
      String _ -> S <$> asSymbol
      _ -> throwCustomError (BadPVal v)

asConfig :: ParseIt [V PVal]
asConfig = eachInArray (asV asPVal)

instance ToJSON Param where
  toJSON (Param pname ptype) = object
    [ "name" .= String pname
    , "type" .= toJSON ptype ]

asParam :: ParseIt Param
asParam = Param <$> key "name" asName <*> key "type" (asV asPType)

instance ToJSON Expr where
  toJSON = String . pretty

asExpr :: ParseIt Expr
asExpr = do
    t <- asText
    case parseExprText t of
      Right e  -> pure e
      Left msg -> throwCustomError (ExprParseError (pack msg) t)

instance ToJSON Fun where
  toJSON (Fun param body) = object
    [ "parameter" .= toJSON param
    , "body"      .= toJSON body ]

asFun :: ParseIt Fun
asFun = Fun <$> key "parameter" asParam <*> key "body" (asV asExpr)

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

asEffect :: ParseIt Effect
asEffect = do
    eff <- key "effect" asText
    case eff of
      "create" -> Create <$> key "expression" (asV asExpr)
      "check"  -> Check  <$> key "function" asFun
      "modify" -> Modify <$> key "function" asFun
      "delete" -> pure Delete
      _ -> throwCustomError (BadCase "effect" ["create","check","modify","delete"] eff)

instance ToJSON Stmt where
  toJSON (Do path eff) = object
    [ "statement" .= String "do"
    , "path"      .= toJSON path
    , "effect"    .= toJSON eff ]
  toJSON (If cond tru fls) = object
    [ "statement" .= String "if"
    , "condition" .= toJSON cond
    , "then"      .= toJSON tru
    , "else"      .= toJSON fls ]
  toJSON (In ctx body) = object
    [ "statement" .= String "in"
    , "context"   .= toJSON ctx
    , "body"      .= toJSON body ]
  toJSON (For x expr body) = object
    [ "statement" .= String "for"
    , "variable"  .= String x
    , "maximum"   .= toJSON expr
    , "body"      .= toJSON body ]
  toJSON (Let x bound body) = object
    [ "statement" .= String "let"
    , "variable"  .= String x
    , "bound"     .= toJSON bound
    , "body"      .= toJSON body ]
  toJSON (Load comp args) = object
    [ "statement" .= String "load"
    , "component" .= toJSON comp
    , "arguments" .= toJSON args ]

asStmt :: ParseIt Stmt
asStmt = do
    stmt <- key "statement" asText
    case stmt of
      "do"   -> Do   <$> key "path"      asPath
                     <*> key "effect"    asEffect
      "if"   -> If   <$> key "condition" (asV asExpr)
                     <*> key "then"      asBlock
                     <*> key "else"      asBlock
      "in"   -> In   <$> key "context"   asPath
                     <*> key "body"      asBlock
      "for"  -> For  <$> key "variable"  asName
                     <*> key "maximum"   (asV asExpr)
                     <*> key "body"      asBlock
      "let"  -> Let  <$> key "variable"  asName
                     <*> key "bound"     (asV asExpr)
                     <*> key "body"      asBlock
      "load" -> Load <$> key "component" (asV asExpr)
                     <*> key "arguments" (eachInArray (asV asExpr))
      _ -> throwCustomError (BadCase "stmt" ["do","in","if","let","load"] stmt)

instance (ToJSON a) => ToJSON (Segment a) where
  toJSON (Elems xs) = object
    [ "segType" .= String "elems"
    , "elems" .= toJSON xs ]
  toJSON (Split d l r) = object
    [ "segType" .= String "split"
    , "dim" .= toJSON d
    , "l" .= toJSON l
    , "r" .= toJSON r ]

asSegment :: ParseIt a -> ParseIt (Segment a)
asSegment asA = do
  segType <- key "segType" asText
  case segType of
    "elems" -> elems
    "split" -> split
    _ -> throwCustomError (BadCase "segType" ["elems", "split"] segType)
  where
    elems = Elems <$> key "elems" (eachInArray asA)
    split = Split
      <$> key "dim" asBExpr
      <*> key "l" (asSegList asA)
      <*> key "r" (asSegList asA)

asSegList :: ParseIt a -> ParseIt (SegList a)
asSegList asA = eachInArray (asSegment asA)

asBlock :: ParseIt Block
asBlock = asSegList asStmt

instance ToJSON Entry where
  toJSON (ProEntry p) = object
    [ "type"  .= String "profile"
    , "entry" .= toJSON p ]
  toJSON (ModEntry m) = object
    [ "type"  .= String "model"
    , "entry" .= toJSON m ]

asEntry :: ParseIt Entry
asEntry = do
    t <- key "type" asText
    case t of
      "profile" -> ProEntry <$> key "entry" asProfile
      "model"   -> ModEntry <$> key "entry" asModel
      _ -> throwCustomError (BadCase "dictionary entry" ["profile","model"] t)

instance (ToJSON k, ToJSON v) => ToJSON (Env k v) where
  toJSON (Env m) = Array (fromList (map entry (toAscList m)))
    where entry (k,v) = object [ "key" .= toJSON k, "value" .= toJSON v ]

asEnv :: (Ord k, MergeDup v) => ParseIt k -> ParseIt v -> ParseIt (Env k v)
asEnv asKey asVal = envFromListAcc <$> eachInArray entry
  where entry = (,) <$> key "key" asKey <*> key "value" asVal

asVarEnv :: ParseIt VarEnv
asVarEnv = asEnv asName (asV (asMaybe asPVal))

asResEnv :: ParseIt ResEnv
asResEnv = asEnv asResID (asV (asMaybe asPVal))

asDictionary :: ParseIt Dictionary
asDictionary = asEnv asSymbol asEntry

instance ToJSON Model where
  toJSON (Model xs block) = object
    [ "parameters" .= toJSON xs
    , "block"      .= toJSON block ]

asModel :: ParseIt Model
asModel = Model
    <$> key "parameters" (eachInArray asParam)
    <*> key "block" asBlock

instance ToJSON Profile where
  toJSON (Profile xs effs) = object
    [ "parameters" .= toJSON xs
    , "effects"    .= toJSON effs ]

asProfile :: ParseIt Profile
asProfile = Profile
    <$> key "parameters" (eachInArray asParam)
    <*> key "effects" (asEnv asPath (asSegList asEffect))

instance ToJSON Error where
  toJSON e = String (pretty e)

instance ToJSON SuccessCtx where
  toJSON (SuccessCtx b vs) = object
    [ "context" .= toJSON b
    , "cfgSpace" .= toJSON vs ]

asSuccess :: ParseIt SuccessCtx
asSuccess = SuccessCtx
    <$> key "context" asBExpr
    <*> key "cfgSpace" (S.fromList <$> eachInArray asText)
