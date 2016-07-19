module DSL.Serialize where

import Data.Aeson (ToJSON,FromJSON,decode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as B
import System.Directory (createDirectoryIfMissing)


-- * Reading and writing JSON files

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
