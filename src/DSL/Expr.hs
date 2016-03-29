{-# LANGUAGE DeriveGeneric #-}

module DSL.Expr where

import Data.String (IsString)
import GHC.Generics (Generic)

import DSL.Env
import DSL.Type


--
-- * Expression Syntax
--

-- | Expressions.
data Expr a
     -- literals
     = Unit                         -- ^ unit value
     | B Bool                       -- ^ boolean literal
     | I Int                        -- ^ integer literal
     -- simply typed lambda calculus
     | Ref Var                      -- ^ non-linear variable reference
     | Use Var                      -- ^ linear variable reference
     | Abs Var (Schema a) (Expr a)  -- ^ lambda abstraction
     | App (Expr a) (Expr a)        -- ^ application
     -- reuse
     | Free (Expr a)                -- ^ mark reusable term
     | Reuse (Expr a) Var (Expr a)  -- ^ use reusable term
     -- records
     | Rec (Row (Expr a))           -- ^ record values
     | Sel Label (Expr a)           -- ^ record selection
     | Res Label (Expr a)           -- ^ record restriction
     | Ext Label (Expr a) (Expr a)  -- ^ record extension
  deriving (Eq,Generic,Show)

-- | Binary function application.
app2 :: Expr a -> Expr a -> Expr a -> Expr a
app2 f x y = App (App f x) y

-- | Build record from association list.
rec :: [(Label, Expr a)] -> Expr a
rec = Rec . row

-- | Record update.
update :: Label -> Expr a -> Expr a -> Expr a
update l e r = Ext l e (Res l r)

-- | Is this term in normal form?
isNormal :: Expr a -> Bool
isNormal Unit        = True
isNormal (B _)       = True
isNormal (I _)       = True
isNormal (Ref _)     = True
isNormal (Use _)     = True
isNormal (Abs x _ _) = True   -- don't isNormalize under abstraction
isNormal (Free e)    = isNormal e
isNormal (Rec r)     = all isNormal r
isNormal _           = False
