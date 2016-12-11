module DSL.Sugar where

import DSL.Effect
import DSL.Expression
import DSL.Model
import DSL.Name
import DSL.Path
import DSL.Primitive


--
-- * Syntactic Sugar
--

-- ** Expressions

-- | Literal component ID.
dfu :: Name -> Expr
dfu = Lit . S . mkSymbol

-- | Primitive floor operation.
pFloor :: Expr -> Expr
pFloor = P1 (F_I Floor)

-- | Primitive ceiling operation.
pCeil :: Expr -> Expr
pCeil = P1 (F_I Ceil)

-- | Primitive round operation.
pRound :: Expr -> Expr
pRound = P1 (F_I Round)


-- ** Statements

-- | Create a resource.
create :: Path -> Expr -> Stmt
create p e = Do p (Create e)

-- | Check a resource.
check :: Path -> PType -> Expr -> Stmt
check p t e = Do p (Check (Fun (Param "$val" t) e))

-- | Modify a resource.
modify :: Path -> PType -> Expr -> Stmt
modify p t e = Do p (Modify (Fun (Param "$val" t) e))

-- | Reference the current value of a resource.
--   For use with the 'check' and 'modify' smart constructors.
val :: Expr
val = Ref "$val"

-- | Check whether a unit-valued resource is present.
checkUnit :: Path -> Stmt
checkUnit p = Do p (Check (Fun (Param "$val" TUnit) true))

-- | Create a unit-valued resource.
createUnit :: Path -> Stmt
createUnit p = Do p (Create (Lit Unit))

-- | Macro for an integer-case construct. Evaluates the expression, then
--   compares the resulting integer value against each case in turn, executing
--   the first matching block, otherwise executes the final block arugment.
caseOf :: Expr -> [(Int,Block)] -> Block -> Stmt
caseOf expr cases other = Let x expr (foldr ifs other cases)
  where
    ifs (i,thn) els = [If (Ref x .== Lit (I i)) thn els]
    x = "$case"
