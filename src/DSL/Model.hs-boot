module DSL.Model where

import Data.Data (Data,Typeable)
import DSL.Environment (MergeDup)

data Model

instance Data     Model
instance Eq       Model
instance Read     Model
instance Show     Model
instance Typeable Model
instance MergeDup Model
