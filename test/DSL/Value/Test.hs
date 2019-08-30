module DSL.Value.Test where

import Test.Tasty
import Test.Tasty.HUnit

import DSL.Boolean
import DSL.Environment
import DSL.Resource
import DSL.Types
import DSL.Value


runOp1 :: Op1 -> V (Maybe PVal) -> (Either Mask (V (Maybe PVal)), StateCtx)
runOp1 o v = runEmpty . unVM $ (applyPrim1 o (VM . return $ v))

testValue = testGroup "DSL.Value" [testApplyPrim1]

testApplyPrim1 = testGroup "applyPrim1"
  [
    testCase "Not (One True) = False" $
      runOp1 (B_B Not) (One (Just (B True))) @?=
      (Right (One (Just (B False))),SCtx {renv = envEmpty, errCtx = BLit False, mask = One Nothing}),
    testCase "Not (One 1) = Error" $
      runOp1 (B_B Not) (One (Just (I 1))) @?=
      (Left (One . Just . PrimE $ ErrorOp1 (B_B Not) (I 1)), SCtx {renv = envEmpty, errCtx = BLit False ||| BLit True, mask = One . Just . PrimE $ ErrorOp1 (B_B Not) (I 1) }),
    testCase "Neg A<1,-1> = A<-1,1>" $
      runOp1 (N_N Neg) (Chc (BRef "A") (One (Just (I 1))) (One (Just (I (-1))))) @?=
      (Right (Chc (BRef "A") (One (Just (I (-1)))) (One (Just (I 1)))),SCtx {renv = envEmpty, errCtx = BLit False, mask = One Nothing}),
    testCase "Neg A<1,True> = A<-1,Error>" $
      runOp1 (N_N Neg) (Chc (BRef "A") (One (Just (I 1))) (One (Just (B True)))) @?=
      (Right (
        Chc
          (BRef "A")
          (One (Just (I (-1))))
          (One Nothing)),
        SCtx {renv = envEmpty, errCtx = OpBB Or (BLit False) (OpBB And (BLit True) (OpB Not (BRef "A"))), mask = Chc (OpBB And (BLit True) (OpB Not (BRef "A"))) (One (Just (PrimE (ErrorOp1 (N_N Neg) (B True))))) (One Nothing)})
  ]
