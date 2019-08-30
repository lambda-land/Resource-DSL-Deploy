-- | A reproduction of SBV's Boolean type class, which we use heavily but
--   was removed in SBV version 8.0.
module DSL.Boolean where

import Data.SBV


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

instance Boolean SBool where
  true = sTrue
  false = sFalse
  bnot = sNot
  (&&&) = (.&&)
  (|||) = (.||)
  (<+>) = (.<+>)
  (==>) = (.=>)
  (<=>) = (.<=>)
