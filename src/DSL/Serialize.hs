module DSL.Serialize where

import Data.Aeson

import DSL.Expr
import DSL.Type


instance ToJSON Basic
instance ToJSON Expr
instance ToJSON Pred
instance ToJSON a => ToJSON (Schema a)
instance ToJSON a => ToJSON (Type a)

instance FromJSON Basic
instance FromJSON Expr
instance FromJSON Pred
instance FromJSON a => FromJSON (Schema a)
instance FromJSON a => FromJSON (Type a)
