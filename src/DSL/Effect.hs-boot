module DSL.Effect where

import Data.Data (Data,Typeable)

data Effect

instance Data     Effect
instance Eq       Effect
instance Read     Effect
instance Show     Effect
instance Typeable Effect
