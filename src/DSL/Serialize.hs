module DSL.Serialize where

import Data.Aeson

import DSL.Expr
import DSL.Type


instance ToJSON Var
instance ToJSON RVar
instance ToJSON Basic
instance ToJSON Qual
instance ToJSON Expr
instance ToJSON a => ToJSON (Schema a)
instance ToJSON a => ToJSON (Type a)

instance FromJSON Var
instance FromJSON RVar
instance FromJSON Basic
instance FromJSON Qual
instance FromJSON Expr
instance FromJSON a => FromJSON (Schema a)
instance FromJSON a => FromJSON (Type a)
