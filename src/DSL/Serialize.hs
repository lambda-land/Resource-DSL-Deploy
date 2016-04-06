module DSL.Serialize where

import Data.Aeson (ToJSON,FromJSON,decode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as B
import System.Directory (createDirectoryIfMissing)

import DSL.Expr
import DSL.Predicate
import DSL.Primitive
import DSL.Type


-- ** Reading and writing JSON files

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
              


-- ** Auto-generated instances

instance ToJSON BPred
instance ToJSON IPred
instance ToJSON Simple
instance ToJSON Refined
instance ToJSON B_B
instance ToJSON I_I
instance ToJSON BB_B
instance ToJSON II_B
instance ToJSON II_I
instance ToJSON Op1
instance ToJSON Op2
instance ToJSON a => ToJSON (Schema a)
instance ToJSON a => ToJSON (Type a)
instance ToJSON a => ToJSON (Expr a)

instance FromJSON BPred
instance FromJSON IPred
instance FromJSON Simple
instance FromJSON Refined
instance FromJSON B_B
instance FromJSON I_I
instance FromJSON BB_B
instance FromJSON II_B
instance FromJSON II_I
instance FromJSON Op1
instance FromJSON Op2
instance FromJSON a => FromJSON (Schema a)
instance FromJSON a => FromJSON (Type a)
instance FromJSON a => FromJSON (Expr a)
