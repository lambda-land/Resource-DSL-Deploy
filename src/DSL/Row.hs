
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

-- | Check whether a row contains a particular label.
hasLabel :: Label -> Row a -> Bool
hasLabel = Map.member

-- | Extend a row with a new entry.
rowExtend :: Label -> a -> Row a -> Row a
rowExtend = Map.insert

-- | Lookup the value associated with a label and remove it from the row.
rowExtract :: Label -> Row a -> Maybe (a, Row a)
rowExtract l r = ma >>= \a -> Just (a,r')
  where (ma,r') = Map.updateLookupWithKey (\_ _ -> Nothing) l r
