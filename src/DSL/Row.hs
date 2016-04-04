
module DSL.Row where

import Data.Map (Map)
import qualified Data.Map as Map


-- | Record labels.
type Label = String

-- | Rows.
type Row a = Map Label a

-- | Smart constructor for rows.
row :: [(Label,a)] -> Row a
row = Map.fromList

-- | Lookup the value associated with a label.
rowLookup :: Label -> Row a -> Maybe a
rowLookup = Map.lookup
