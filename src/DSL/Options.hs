module DSL.Options where

import Data.Aeson
import Data.String
import Data.ByteString.Lazy ()
import Options.Applicative

import qualified DSL.Types as T
import DSL.Pretty ()
import DSL.Parser

readRecord :: (FromJSON a) => ReadM a
readRecord = eitherReader $ \arg -> case decode (fromString arg) of
  Just r -> return r
  _      -> Left $ "cannot parse value `" ++ arg ++ "'"

instance FromJSON T.Value where
  parseJSON = withText "Value" (\x -> case parseValueText x of
                                        Right v -> return v
                                        Left s -> fail s)
