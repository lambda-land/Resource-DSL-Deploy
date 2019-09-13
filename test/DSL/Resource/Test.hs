module DSL.Resource.Test where

import Test.Tasty
import Test.Tasty.HUnit
import DSL.Types
import DSL.Evaluation
import DSL.Environment

testValue = testGroup "DSL.Resource" [testApplyPrim1]

testApplyPrim1 = testGroup "applyPrim1"
