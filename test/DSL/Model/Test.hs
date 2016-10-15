module DSL.Model.Test where

import           Control.Exception.Base (catch)
import           Test.Tasty
import           Test.Tasty.HUnit

import           DSL.Effect
import           DSL.Environment
import           DSL.Expression
import           DSL.Primitive
import           DSL.Profile
import           DSL.Resource
import           DSL.Model 

-- ** Helper functions
checkFor :: Name -> Effect
checkFor n = Check (Fun (P n TUnit) true)

create :: Effect
create = Create $ Lit Unit

blockTests :: Block
blockTests = [provideUnit "x", checkUnit "x", In ["foo"] [checkUnit "x"]]
--
-- ** Tests

testModel :: TestTree
testModel = testGroup "Model Tests"
  [ testGroup "test toProfile"

    [ testCase "toProfile converts a Model to a Profile, with checkUnit" $
      do out <- return $ toProfile (Model [P "x" TInt] [(checkUnit "x")])
         p <- return $ Profile [P "x" TInt]
           (envFromList [(["x"], [checkFor "x"])]) 
         p @=? out

    , testCase "toProfile converts a Model to a Profile, with a Block" $
      do out <- return $ toProfile (Model [P "x" TInt] [provideUnit "x"
                                                       , checkUnit "x"])
         p <- return $ Profile [P "x" TInt]
           (envFromList [(["x"], [create, checkFor "x"])]) 
         p @=? out

    , testCase "toProfile creates a Profile wih an In block" $
      do out <- return $ toProfile (Model [P "x" TInt] blockTests)
         p <- return $ Profile [P "x" TInt]
           (envFromList [(["foo", "x"], [checkFor "x"])
                        ,(["x"], [create , checkFor "x"])])
         p @=? out

    -- TODO: Test toProfile with If Let and Load statements
    ]

  -- , [ testCase "test profileDict" $
  --     do out <- return $ profileDict [("x", Model [P "y" TInt]
  --                                      [provideUnit "x"])]
  --        p <- return $ P
  --   ]
  ]
