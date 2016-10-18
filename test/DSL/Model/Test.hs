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

-- | blockone tests Do stmts and an In stmt
blockone :: Block
blockone = [provideUnit "x", checkUnit "x", In ["foo"] [checkUnit "x"]]

blocktwo :: Block
blocktwo = [If ((Lit (B True)) .== (Lit (B True)))
            [Let "x" (Lit (I 1)) []]
            [Let "x" (Lit (I 0)) []]
           , Load "loading"
             [Ref "a", Lit (I 1729), (P1 (I_I Neg) (Lit (I 1729)))]]

blockthree :: Block
blockthree = init blockone

runLoadModel :: Model -> [Expr] -> ResEnv -> IO ResEnv
runLoadModel m e env = snd <$> runInEmptyContext env (loadModel m e)

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
      do out <- return $ toProfile (Model [P "x" TInt] blockone)
         p <- return $ Profile [P "x" TInt]
           (envFromList [(["foo", "x"], [checkFor "x"])
                        ,(["x"], [create , checkFor "x"])])
         p @=? out

    -- TODO: Test toProfile with If Let and Load statements
    ]

  , testGroup "test profileDict"

    [ testCase "profileDict creates a Dict with single Effect" $
      do out <- return $ profileDict [("x", Model [P "y" TInt]
                                       [provideUnit "x"])]
         p <- return $ envFromList [("x"
                                    , ProEntry
                                      (Profile [P "y" TInt]
                                       (envSingle ["x"]
                                        [(Create (Lit Unit))])))]
         p @=? out
    ]

  , testGroup "test composeModels"

    [ testCase "composeModels unions two Models with distinct Params" $
      do
      let m1 = Model [P "x" TInt, P "y" TBool] blockone
      let m2 = Model [P "z" TUnit] blockone
      out <- return $ composeModels m1 m2
      p <- return $ Model [P "x" TInt, P "y" TBool, P "z" TUnit]
        (blockone ++ blockone)
      p @=? out

    , testCase "composeModels choose left Param where Params shadow each other" $
      do
      let m1 = Model [P "x" TInt, P "y" TBool, P "z" TUnit] blockone
      let m2 = Model [P "x" TInt, P "y" TBool, P "z" TUnit] blockone
      out <- return $ composeModels m1 m2
      p <- return $ Model [P "x" TInt, P "y" TBool, P "z" TUnit]
        (blockone ++ blockone)
      p @=? out

    , testCase "composeModels considers PType when composing" $
      do
      let m1 = Model [P "x" TInt, P "y" TBool] blockone
      let m2 = Model [P "x" TUnit, P "y" TUnit] blockone
      out <- return $ composeModels m1 m2
      p <- return $ Model [P "x" TInt, P "y" TBool, P "x" TUnit, P "y" TUnit]
        (blockone ++ blockone)
      p @=? out
    ]

  , testGroup "test loadModel" 
    [ testCase "load model loads a model" $
      do out <- runLoadModel (Model [P "ref_x" TInt, P "ref_y" TBool] blockthree)
           [Lit (I 1), Lit (B True)]
           envEmpty
           -- (envFromList [(["ref x"], I 10)])
         envFromList [(["x"], Unit)] @=? out
    ]
  ]
