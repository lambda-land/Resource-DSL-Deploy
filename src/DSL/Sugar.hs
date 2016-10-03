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


-- ** Statements

-- | Check whether a unit-valued resource is present.
checkUnit :: Path -> Stmt
checkUnit p = Do p (Check (Fun (Param "x" TUnit) true))

-- | Provide a unit-valued resource.
provideUnit :: Path -> Stmt
provideUnit p = Do p (Create (Lit Unit))

-- | Macro for an integer-case construct. Evaluates the expression, then
--   compares the resulting integer value against each case in turn, executing
--   the first matching block, otherwise executes the final block arugment.
caseOf :: Expr -> [(Int,Block)] -> Block -> Stmt
caseOf expr cases other = Let x expr (foldr ifs other cases)
  where
    ifs (i,thn) els = [If (Ref x .== Lit (I i)) thn els]
    x = "$case"
