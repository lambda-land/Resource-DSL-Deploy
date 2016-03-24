module DSL.Serialize where

import Data.Aeson
import DSL.Type

instance ToJSON Basic
instance ToJSON Qual
instance ToJSON a => ToJSON (Schema a)
instance ToJSON a => ToJSON (Type a)

instance FromJSON Basic
instance FromJSON Qual
instance FromJSON a => FromJSON (Schema a)
instance FromJSON a => FromJSON (Type a)
