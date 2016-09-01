module DSL.Model where

import Data.Data (Data,Typeable)

data Model

instance Data     Model
instance Eq       Model
instance Read     Model
instance Show     Model
instance Typeable Model
