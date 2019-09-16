module DSL.Sugar where

import qualified Data.Text as T
import qualified Data.Set as S

import DSL.Boolean
import DSL.Types
import DSL.Parser


--
-- * Syntactic Sugar
--

-- ** Variational values

-- | Construct a binary choice in the given dimension.
chc :: Var -> a -> a -> V a
chc d l r = Chc (BRef d) (One l) (One r)

-- | Construct an n-ary choice by cascading binary choices drawn from the
--   given list of dimensions.
chcN :: [Var] -> [a] -> V a
chcN _      [a]    = One a
chcN (d:ds) (a:as) = Chc (BRef d) (One a) (chcN ds as)
chcN _ _ = error "chcN: illegal arguments"


-- ** Variational blocks

-- | Construct a variational statement by branching on a choice.
split :: Var -> Block -> Block -> Stmt
split d = If (chc d true false)

-- | Construct an n-ary variational block by cascading if-statement drawn from the
--   given list of dimensions.
splitN :: [Var] -> [Block] -> Block
splitN _      [b]    = b
splitN (d:ds) (b:bs) = [split d b (splitN ds bs)]
splitN _ _ = error "splitN: illegal arguments"


-- ** Expressions

-- | Non-variational literals.
lit :: PVal -> V Expr
lit = One . Lit . One

-- | Non-variational integer.
int :: Int -> V Expr
int = lit . I

-- | Literal string.
str :: Name -> V Expr
str = lit . S

-- | Non-variational variable reference.
ref :: Var -> V Expr
ref = One . Ref

-- | Non-variational resource reference.
res :: Path -> V Expr
res = One . Res

-- | Primitive floor operation.
pFloor :: V Expr -> Expr
pFloor = P1 (F_I Floor)

-- | Primitive ceiling operation.
pCeil :: V Expr -> Expr
pCeil = P1 (F_I Ceil)

-- | Primitive round operation.
pRound :: V Expr -> Expr
pRound = P1 (F_I Round)

-- | Conditional expressions.
(??) :: V Expr -> (V Expr, V Expr) -> Expr
c ?? (t,e) = P3 Cond c t e

infix 1 ??


-- ** Statements

-- | Create a resource.
create :: Path -> V Expr -> Stmt
create p e = Do p (Create e)

-- | Check a resource.
check :: Path -> PType -> V Expr -> Stmt
check p t e = Do p (Check (Fun (Param "$val" t) e))

-- | Modify a resource.
modify :: Path -> PType -> V Expr -> Stmt
modify p t e = Do p (Modify (Fun (Param "$val" t) e))

-- | Reference the current value of a resource.
--   For use with the 'check' and 'modify' smart constructors.
val :: V Expr
val = One (Ref "$val")

-- | Check whether a unit-valued resource is present.
checkUnit :: Path -> Stmt
checkUnit p = Do p (Check (Fun (Param "$val" TUnit) true))

-- | Create a unit-valued resource.
createUnit :: Path -> Stmt
createUnit p = Do p (Create (One . Lit . One $ Unit))

mkVExpr :: PVal -> V Expr
mkVExpr = One . Lit . One

dim :: T.Text -> BExpr
dim t | Right b <- parseBExprText t = b
      | Left s <- parseBExprText t = error s
      | otherwise = error "impossible"

exclusive' :: S.Set T.Text -> S.Set T.Text -> Maybe BExpr
exclusive' all yes = combine (yesExpr yesList) (noExpr noList)
  where
    no = S.difference all yes
    noList = fmap (\t -> (bnot (BRef t))) (S.toList no)

    noExpr :: [BExpr] -> Maybe BExpr
    noExpr [] = Nothing
    noExpr (x:xs) | Just b <- noExpr xs = Just (x &&& b)
                  | otherwise           = Just x

    yesList = fmap BRef (S.toList yes)

    yesExpr :: [BExpr] -> Maybe BExpr
    yesExpr [] = Nothing
    yesExpr (x:xs) | Just b <- yesExpr xs = Just (x ||| b)
                   | otherwise            = Just x

    combine :: Maybe BExpr -> Maybe BExpr -> Maybe BExpr
    combine (Just y) (Just n) = Just (y &&& n)
    combine Nothing  (Just n) = Just n
    combine (Just y) Nothing  = Just y
    combine _        _        = Nothing

exclusive :: [T.Text] -> [T.Text] -> Maybe BExpr
exclusive all yes = exclusive' (S.fromList all) (S.fromList yes)

(.==.) :: V Expr -> V Expr -> V Expr
x .==. y = (One (P2 (SS_B SEqu) x y))
