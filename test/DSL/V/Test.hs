module DSL.V.Test where

import Test.Tasty
import Test.Tasty.HUnit
import DSL.Types
import DSL.V
import Data.SBV (bnot, (&&&), (|||))

testV = testGroup "DSL.V" [testSelect, testMergeMask]

-- | A type-constrained version of 'select'.
selectV :: BExpr -> V Expr -> V Expr
selectV = select

testSelect = testGroup "select"
  [
    testCase "Select on One produces One" $
      selectV (BRef "A") (One 1) @?= One 1,
    testCase "Select on equal dimensions" $
      selectV (BRef "A")
        (Chc (BRef "B")
          (Chc (BRef "A")
            (One 1)
            (One 2)
          )
          (One 3)
        ) @?= (Chc (BRef "B") (One 1) (One 3)),
    testCase "Select on opposite dimensions" $
      selectV (bnot (BRef "A"))
        (Chc (BRef "B")
          (Chc (BRef "A")
            (One 1)
            (One 2)
          )
          (One 3)
        ) @?= (Chc (BRef "B") (One 2) (One 3)),
    testCase "Select on implied dimensions" $
      selectV (BRef "A" &&& BRef "C")
        (Chc (BRef "B")
          (Chc (BRef "A")
            (One 1)
            (One 2)
          )
          (One 3)
        ) @?= (Chc (BRef "B") (One 1) (One 3)),
    testCase "Select on negated implied dimensions" $
      selectV (bnot (BRef "A"))
        (Chc (BRef "B")
          (Chc (BRef "A" &&& BRef "C")
            (One 1)
            (One 2)
          )
          (One 3)
        ) @?= (Chc (BRef "B") (One 2) (One 3))
  ]

testMergeMask = testGroup "mergeMask"
  [
    testCase "A single error merges into leaves of chc tree" $
      mergeMask
        (Chc (BRef "B")
          (Chc (BRef "A")
            (One . Just . PrimE $ ErrorOp1 U_U (I 1))
            (One Nothing)
          )
          (One . Just . PrimE $ ErrorOp1 U_U (I 2))
        )
        (One . Just . PrimE $ ErrorOp1 U_U (I 3)) @?=
        (Chc (BRef "B")
          (Chc (BRef "A")
            (One . Just . PrimE $ ErrorOp1 U_U (I 1))
            (One . Just . PrimE $ ErrorOp1 U_U (I 3))
          )
          (One . Just . PrimE $ ErrorOp1 U_U (I 2))
        ),
    testCase "Merge into equiv dimensions" $
      mergeMask
        (Chc (BRef "B")
          (Chc (BRef "A")
            (One . Just . PrimE $ ErrorOp1 U_U (I 1))
            (One Nothing)
          )
          (One Nothing)
        )
        (Chc (BRef "B")
            (One . Just . PrimE $ ErrorOp1 U_U (I 2))
            (One . Just . PrimE $ ErrorOp1 U_U (I 3))
        ) @?=
        (Chc (BRef "B")
          (Chc (BRef "A")
            (One . Just . PrimE $ ErrorOp1 U_U (I 1))
            (One . Just . PrimE $ ErrorOp1 U_U (I 2))
          )
          (One . Just . PrimE $ ErrorOp1 U_U (I 3))
        ),
    testCase "Merge with d |<=| d'" $
      mergeMask
        (Chc (BRef "B")
          (Chc (BRef "A")
            (One . Just . PrimE $ ErrorOp1 U_U (I 1))
            (One Nothing)
          )
          (One Nothing)
        )
        (Chc (BRef "B" &&& BRef "C")
            (One . Just . PrimE $ ErrorOp1 U_U (I 2))
            (One . Just . PrimE $ ErrorOp1 U_U (I 3))
        ) @?=
        (Chc (BRef "B")
          (Chc (BRef "A")
            (One . Just . PrimE $ ErrorOp1 U_U (I 1))
            (Chc (BRef "B" &&& BRef "C")
                (One . Just . PrimE $ ErrorOp1 U_U (I 2))
                (One . Just . PrimE $ ErrorOp1 U_U (I 3))
            )
          )
          (One Nothing)
        ),
    testCase "Merge with d |!<=| d'" $
      mergeMask
        (Chc (BRef "B")
          (Chc (BRef "A")
            (One . Just . PrimE $ ErrorOp1 U_U (I 1))
            (One Nothing)
          )
          (One Nothing)
        )
        (Chc ((bnot (BRef "B")) &&& BRef "C")
            (One . Just . PrimE $ ErrorOp1 U_U (I 2))
            (One . Just . PrimE $ ErrorOp1 U_U (I 3))
        ) @?=
        (Chc (BRef "B")
          (Chc (BRef "A")
            (One . Just . PrimE $ ErrorOp1 U_U (I 1))
            (One Nothing)
          )
          (Chc ((bnot (BRef "B")) &&& BRef "C")
              (One . Just . PrimE $ ErrorOp1 U_U (I 2))
              (One . Just . PrimE $ ErrorOp1 U_U (I 3))
          )
        ),
    testCase "Merge with d |=>| d'" $
      mergeMask
        (Chc (BRef "B" &&& BRef "C")
          (Chc (BRef "A")
            (One . Just . PrimE $ ErrorOp1 U_U (I 1))
            (One Nothing)
          )
          (One Nothing)
        )
        (Chc (BRef "B")
            (One . Just . PrimE $ ErrorOp1 U_U (I 2))
            (One . Just . PrimE $ ErrorOp1 U_U (I 3))
        ) @?=
        (Chc (BRef "B")
          (Chc (BRef "B" &&& BRef "C")
            (Chc (BRef "A")
              (One . Just . PrimE $ ErrorOp1 U_U (I 1))
              (One . Just . PrimE $ ErrorOp1 U_U (I 2))
            )
            (One . Just . PrimE $ ErrorOp1 U_U (I 2))
          )
          (One . Just . PrimE $ ErrorOp1 U_U (I 3))
        ),
    testCase "Merge with d |=>!| d'" $
      mergeMask
        (Chc (bnot (BRef "B"))
          (Chc (BRef "A")
            (One . Just . PrimE $ ErrorOp1 U_U (I 1))
            (One Nothing)
          )
          (One Nothing)
        )
        (Chc (BRef "B" &&& BRef "C")
            (One . Just . PrimE $ ErrorOp1 U_U (I 2))
            (One . Just . PrimE $ ErrorOp1 U_U (I 3))
        ) @?=
        (Chc (bnot (BRef "B"))
          (Chc (BRef "A")
            (One . Just . PrimE $ ErrorOp1 U_U (I 1))
            (One Nothing)
          )
          (Chc (BRef "B" &&& BRef "C")
              (One . Just . PrimE $ ErrorOp1 U_U (I 2))
              (One . Just . PrimE $ ErrorOp1 U_U (I 3))
          )
        )
  ]
