module DSL.Profile where

import Data.Data (Data,Typeable)
import DSL.Environment (MergeDup)

data Profile

instance Data     Profile
instance Eq       Profile
instance Read     Profile
instance Show     Profile
instance Typeable Profile
instance MergeDup Profile
