{-# LANGUAGE DeriveGeneric #-}

module DSL.Expr where

import Data.String (IsString)
import GHC.Generics (Generic)

import DSL.Env
import DSL.Type


-- | Expressions.
data Expr
     -- literals
     = Unit                 -- ^ unit value
     | B Bool               -- ^ boolean literal
     | I Int                -- ^ integer literal
     -- lambda calculus
     | Ref Var              -- ^ variable reference
     | Abs Var Expr         -- ^ lambda abstraction
     | App Expr Expr        -- ^ application
     -- reuse
     | Free Expr            -- ^ mark reusable term
     | Reuse Expr Var Expr  -- ^ use reusable term
     -- records
     | Rec (Row Expr)       -- ^ record values
     | Sel Label Expr       -- ^ record selection
     | Res Label Expr       -- ^ record restriction
     | Ext Label Expr Expr  -- ^ record extension
  deriving (Eq,Generic,Show)

-- | Binary function application.
app2 :: Expr -> Expr -> Expr -> Expr
app2 f x y = App (App f x) y

-- | Build record from association list.
rec :: [(Label,Expr)] -> Expr
rec = Rec . row

-- | Record update.
update :: Label -> Expr -> Expr -> Expr
update l e r = Ext l e (Res l r)
