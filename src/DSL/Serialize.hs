module DSL.Serialize where

import Data.Aeson

import DSL.Expr
import DSL.Predicate
import DSL.Type


instance ToJSON Expr
instance ToJSON BPred
instance ToJSON IPred
instance ToJSON Simple
instance ToJSON Refined
instance ToJSON OpBB
instance ToJSON OpIB
instance ToJSON OpII
instance ToJSON a => ToJSON (Schema a)
instance ToJSON a => ToJSON (Type a)

instance FromJSON Expr
instance FromJSON BPred
instance FromJSON IPred
instance FromJSON Simple
instance FromJSON Refined
instance FromJSON OpBB
instance FromJSON OpIB
instance FromJSON OpII
instance FromJSON a => FromJSON (Schema a)
instance FromJSON a => FromJSON (Type a)
