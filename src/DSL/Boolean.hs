-- | A shared interface for boolean values and operations. This is a
--   reproduction of SBV's pre-8.0 Boolean type class.
module DSL.Boolean where

class Boolean b where
  true  :: b
  false :: b
  bnot  :: b -> b
  (&&&) :: b -> b -> b
  (|||) :: b -> b -> b
  (<+>) :: b -> b -> b
  (==>) :: b -> b -> b
  (<=>) :: b -> b -> b
  fromBool :: Bool -> b

  -- default definitions
  false   = bnot true
  a ||| b = bnot (bnot a &&& bnot b)
  a <+> b = (a &&& bnot b) ||| (bnot a &&& b)
  a <=> b = (a &&& b) ||| (bnot a &&& bnot b)
  a ==> b = bnot a ||| b
  fromBool True  = true
  fromBool False = false

infixl 6 <+>
infixr 3 &&&
infixr 2 |||
infixr 1 ==>, <=>

instance Boolean Bool where
  true  = True
  false = False
  bnot  = not
  (&&&) = (&&)
  (|||) = (||)
