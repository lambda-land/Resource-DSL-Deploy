module DSL.Name where

import Data.Typeable

import Data.String (IsString(..))


--
-- * Names and Symbols
--

-- | Miscellaneous name.
type Name = String

-- | Variable name.
type Var = Name

-- | Named symbols, a la Lisp. Symbol names may contain '_' or '-' in addition
--   to alpha-numeric characters. Symbols are printed as ':'-prefixed strings.
newtype Symbol = Symbol Name
  deriving (Eq,Ord,Show,Typeable)

-- | Get the name of a symbol.
toName :: Symbol -> Name
toName (Symbol n) = ':' : n

-- | Construct a symbol from a string. Strips a ':'-prefix if it exists.
--   Does not check whether the symbol contains only valid characters.
mkSymbol :: Name -> Symbol
mkSymbol (':':n) = Symbol n
mkSymbol n       = Symbol n

-- | Component (DFU) IDs are symbols.
type CompID = Symbol

instance IsString Symbol where
  fromString = mkSymbol
