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
data Expr t
     -- literals
     = Unit                         -- ^ unit value
     | B Bool                       -- ^ boolean literal
     | I Int                        -- ^ integer literal
     -- simply typed lambda calculus
     | Ref Var                      -- ^ non-linear variable reference
     | Use Var                      -- ^ linear variable reference
     | Abs Var (Schema t) (Expr t)  -- ^ lambda abstraction
     | App (Expr t) (Expr t)        -- ^ application
     -- reuse
     | Free (Expr t)                -- ^ mark reusable term
     | Reuse (Expr t) Var (Expr t)  -- ^ use reusable term
     -- records
     | Rec (Row (Expr t))           -- ^ record values
     | Sel Label (Expr t)           -- ^ record selection
     | Res Label (Expr t)           -- ^ record restriction
     | Ext Label (Expr t) (Expr t)  -- ^ record extension
  deriving (Eq,Generic,Show)

-- | Binary function application.
app2 :: Expr t -> Expr t -> Expr t -> Expr t
app2 f x y = App (App f x) y

-- | Build record from association list.
rec :: [(Label, Expr t)] -> Expr t
rec = Rec . row

-- | Record update.
update :: Label -> Expr t -> Expr t -> Expr t
update l e r = Ext l e (Res l r)

-- | Is this term in normal form?
isNormal :: Expr t -> Bool
isNormal Unit        = True
isNormal (B _)       = True
isNormal (I _)       = True
isNormal (Ref _)     = True
isNormal (Use _)     = True
isNormal (Abs x _ _) = True   -- don't isNormalize under abstraction
isNormal (Free e)    = isNormal e
isNormal (Rec r)     = all isNormal r
isNormal _           = False
