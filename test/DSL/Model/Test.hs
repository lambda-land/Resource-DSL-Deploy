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
import           DSL.Name
import           DSL.Path

-- ** Helper functions
checkFor :: Name -> Effect
checkFor n = Check (Fun (Param n TUnit) true)

funAddThree :: Fun
funAddThree = Fun (Param "x" TInt) (Lit (I 3) + Ref "x")

create :: Effect
create = Create $ Lit Unit

checkUnit :: Name -> Stmt
checkUnit n = Do (Path Relative [n]) (Check (Fun (Param "x" TUnit) true))

provideUnit :: Name -> Stmt
provideUnit n = Do (Path Relative [n]) (Create (Lit Unit))

-- | blockone tests Do stmts and an In stmt
blockone :: Block
blockone = [provideUnit "x"
           , checkUnit "x"
           , In (Path Relative ["foo"]) [checkUnit "x"]]

blocktwo :: Block
blocktwo = [If (Lit (B True) .== Lit (B True))
            [Let "x" (Lit (I 1)) []]
            [Let "x" (Lit (I 0)) []]
           , Load (Ref "loading")
             [Ref "a", Lit (I 1729), P1 (I_I Neg) (Lit (I 1729))]]

blockthree :: Block
blockthree = init blockone

runLoadModel :: Model -> [Expr] -> ResEnv -> IO ResEnv
runLoadModel m e env = snd <$> runInEmptyContext env (loadModel m e)

model :: Model
model = Model [Param "y" TInt] [provideUnit "x"]

ctx :: Context
ctx = Ctx (ResID ["foo"])
  (envFromList [("s", I 15)])
  (profileDict [("submodel", model)])

ctx' :: Context
ctx' = Ctx (ResID ["foo"])
  (envFromList [("s", I 15), ("xx", B False)])
  (envSingle "subprofile" (ProEntry
                           (profile [Param "p" TInt ,Param "s" TBool]
                            [(Path Relative ["foo"], [Create (Ref "xx")])])))
--
-- ** Tests

testModel :: TestTree
testModel = testGroup "Model Tests"
  [ testGroup "test toProfile"

    [ testCase "toProfile converts a Model to a Profile, with checkUnit" $
      do let out = toProfile (Model [Param "x" TInt] [checkUnit "x"])
         let p = Profile [Param "x" TInt]
               (envFromList [(Path Relative [".", "x"], [checkFor "x"])])
         p @=? out

    , testCase "toProfile converts a Model to a Profile, with a Block" $
      do let out = toProfile (Model [Param "x" TInt] [provideUnit "x"
                                                     , checkUnit "x"])
         let p = Profile [Param "x" TInt]
               (envFromList [(Path Relative [".", "x"], [create, checkFor "x"])])
         p @=? out

    , testCase "toProfile creates a Profile wih an In block" $
      do let out = toProfile (Model [Param "x" TInt] blockone)
         let p = Profile [Param "x" TInt]
               (envFromList [(Path Relative [".", "foo", "x"], [checkFor "x"])
                            , (Path Relative [".", "x"]
                              , [create , checkFor "x"])])
         p @=? out

    -- TODO: Test toProfile with If Let and Load statements
    -- TODO: Test caseOf
    ]

  , testGroup "test profileDict"

    [ testCase "profileDict creates a Dict with single Effect" $
      do let out = profileDict [("x", Model [Param "y" TInt]
                                       [provideUnit "x"])]
         let p = envFromList [("x", ProEntry
                                      (Profile [Param "y" TInt]
                                       (envSingle (Path Relative [".", "x"])
                                        [(Create (Lit Unit))])))]
         p @=? out
    ]

  , testGroup "test composeModels"

    [ testCase "composeModels unions two Models with distinct Params" $
      do
        let m1 = Model [Param "x" TInt, Param "y" TBool] blockone
        let m2 = Model [Param "z" TUnit] blockone
        let out = composeModels m1 m2
        let p = Model [Param "x" TInt, Param "y" TBool, Param "z" TUnit]
              (blockone ++ blockone)
        p @=? out

    , testCase "composeModels choose left Param where Params shadow each other" $
      do
        let m1 = Model [Param "x" TInt, Param "y" TBool, Param "z" TUnit] blockone
        let m2 = Model [Param "x" TInt, Param "y" TBool, Param "z" TUnit] blockone
        let out = composeModels m1 m2
        let p = Model [Param "x" TInt, Param "y" TBool, Param "z" TUnit]
              (blockone ++ blockone)
        p @=? out

    , testCase "composeModels considers PType when composing" $
      do
        let m1 = Model [Param "x" TInt, Param "y" TBool] blockone
        let m2 = Model [Param "x" TUnit, Param "y" TUnit] blockone
        out <- return $ composeModels m1 m2
        p <- return $ Model [Param "x" TInt, Param "y" TBool, Param "x" TUnit
                            , Param "y" TUnit]
          (blockone ++ blockone)
        p @=? out
    ]

  , testGroup "test loadModel"
    [ testCase "load model loads a model with a do block" $
      do out <- runLoadModel (Model [Param "ref_x" TInt, Param "ref_y" TBool]
                              [provideUnit "x"]) [Lit (I 1), Lit (B True)]
           envEmpty
         envFromList [(ResID ["x"], Unit)] @=? out

    , testCase "load model loads a model with an In block" $
      do out <- runLoadModel
           (Model [Param "ref_x" TInt, Param "ref_y" TBool]
             [In (Path Absolute ["foo"]) blockthree])
           [Lit (I 1), Lit (B True)]
           envEmpty
           -- (envFromList [(["ref x"], I 10)])
         envFromList [(ResID ["foo", "x"], Unit)] @=? out

    , testCase "load model loads a model with an If block, and a ResEnv" $
      do out <- runLoadModel
           (Model [Param "ref_x" TInt, Param "ref_y" TBool] [If (P2 (II_B LTE)
                                                         (Lit (I 1)) (Lit (I 2)))
                                                     blockone
                                                     blocktwo])
           [Lit (I 1), Lit (B True)]
           (envFromList [(ResID ["foo", "x"], Unit)])
         envFromList [(ResID ["foo", "x"], Unit), (ResID ["x"], Unit)] @=? out

    , testCase "load model loads a model with a Let block" $
      do out <- runLoadModel
           (Model [Param "ref_x" TInt, Param "ref_y" TBool] [Let "y" (Lit (I 1))
                                                     blockone])
           [Lit (I 1), Lit (B True)]
           (envFromList [(ResID ["xx"], I 23), (ResID ["foo", "x"], Unit)])

         out @=? envFromList [(ResID ["foo","x"],Unit)
                     , (ResID ["x"],Unit)
                     , (ResID ["xx"],I 23)]

    , testCase "load model loads a model with a Load block, model" $
      do out <- snd <$> runInContext ctx
           (envFromList [(ResID ["submodel"], I 23), (ResID["foo", "x"], Unit)])
           (loadModel (Model [Param "ref_x" TInt, Param "ref_y" TBool]
                       [Load (Ref "submodel") [(+) (Lit $ I 3) (Lit $ I 3)]])
           [Lit (I 1), Lit (B True)])

         out @=? envFromList [(ResID ["foo","x"], Unit)
                     , (ResID ["x"],Unit)
                     , (ResID ["submodel"],I 23)]

    , testCase "load model loads a model with a Load block, profile" $
      do out <- snd <$> runInContext ctx'
           (envFromList [(ResID ["subprofile"], I 23)
                        , (ResID ["foo", "y"], Unit)])
           (loadModel (Model [Param "ref_x" TInt, Param "ref_y" TBool]
                       [Load (Ref "subprofile") [(+) (Lit $ I 3) (Lit $ I 3)]])
           [Lit (I 1), Lit (B True)])
         out @=? envFromList [(ResID ["foo"], B False)
                     , (ResID ["foo", "y"], Unit)
                     , (ResID ["subprofile"],I 23)]
    ]
  ]
