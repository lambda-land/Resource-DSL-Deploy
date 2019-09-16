module DSL.Primitive.Test where

import Test.Tasty
import Test.Tasty.HUnit
import DSL.Types
import DSL.Primitive

testPrim = testGroup "DSL.Primitive" [testPrimOp1, testPrimOp2, testPrimOp3]

testPrimOp1 = testGroup "primOp1"
  [
    testCase "neg 1 = -1" $
      primOp1 (N_N Neg) (I 1) @?= Right (I (-1)),
    testCase "floor True throws an error" $
      primOp1 (F_I Floor) (B True) @?= Left (PrimTypeError1 (F_I Floor) (B True))
  ]

testPrimOp2 = testGroup "primOp2"
  [
    testCase "1+1 = 2" $
      primOp2 (NN_N Add) (I 1) (I 1) @?= Right (I 2),
    testCase "True && 5 throws an error" $
      primOp2 (BB_B And) (B True) (I 5) @?= Left (PrimTypeError2 (BB_B And) (B True) (I 5))
  ]

testPrimOp3 = testGroup "primOp3"
  [
    testCase "if True then 1 else 2 = 1" $
      primOp3 Cond (B True) (I 1) (I 2) @?= Right (I 1),
    testCase "if 1 then 1 else 2 throws an error" $
      primOp3 Cond (I 1) (I 1) (I 2) @?= Left (PrimTypeError3 Cond (I 1) (I 1) (I 2))
  ]
