module DSL.Profile.Test where

import           Control.Exception.Base (catch)
import           Test.Tasty
import           Test.Tasty.HUnit

import           DSL.Effect
import           DSL.Path
import           DSL.Name
import           DSL.Environment
import           DSL.Expression
import           DSL.Primitive
import           DSL.Profile
import           DSL.Resource
import           DSL.Model

-- ** Helper functions
funAddThree :: Fun
funAddThree = Fun (Param "x" TInt) (Lit (I 3) + Ref "x")

ctx' :: Context
ctx' = Ctx (ResID ["foo"])
  (envFromList [("s", I 15), ("xx", I 10), ("y", I 1)])
  (envSingle "subprofile" (ProEntry (profile [Param "p" TInt ,Param "s" TInt]
                                     [(Path Absolute ["foo"]
                                      , [Create (Ref "xx")])])))
--

testProfile :: TestTree
testProfile = testGroup "Test Profile module"
  [ testGroup "Profile tests"
    [ testCase "profile func can create a simple profile" $
      do out <- return $
           profile [Param "p" TInt ,Param "s" TBool] [(Path Absolute ["foo"]
                                                      , [Create (Ref "xx")])]
         p <- return $
           Profile [Param "p" TInt ,Param "s" TBool]
           (envFromList [(Path Absolute ["foo"], [Create (Ref "xx")])])
         p @=? out

    , testCase "profile func can create a profile with more Effects" $
      do out <- return $
           profile [Param "p" TInt ,Param "s" TBool] [(Path Absolute ["foo"]
                                                      , [Create (Ref "x")
                                                        , Check funAddThree
                                                        , Modify funAddThree])]
         p <- return $
           Profile [Param "p" TInt ,Param "s" TBool]
           (envFromList [(Path Absolute ["foo"], [Create (Ref "x")
                                   , Check funAddThree
                                   , Modify funAddThree])])
         p @=? out
    ]

  , testGroup "loadProfile tests"
    [ testCase "loadProfile can load a profile" $
      do let p = profile [Param "p" TInt ,Param "ss" TInt] [(Path Absolute ["bar"]
                                                    , [Create (Ref "s")])]
         let expr = [Ref "y", Lit (I 538)]
         let res = envFromList [(ResID ["foo"], Unit)]
         out <- snd <$> runInContext ctx' res (loadProfile p expr)
         t <- return $ envFromList [(ResID ["foo"], Unit), (ResID ["bar"], I 15)]
         t @=? out
    ]

  , testGroup "composeProfiles test"
    [ testCase "composeProfile can compose profiles" $
      do let p1 = profile [Param "p" TInt ,Param "ss" TInt] [(Path Absolute ["bar"]
                                                     , [Create (Ref "s")])]
         let p2 = profile [Param "x" TBool, Param "a" TUnit] [(Path Absolute ["foo"]
                                                     , [Check funAddThree])]
         out <- return $ composeProfiles p1 p2
         out @=? profile
           [Param "p" TInt, Param "ss" TInt, Param "x" TBool, Param "a" TUnit]
           [(Path Absolute ["foo"], [Check funAddThree]), (Path Absolute ["bar"]
                                                          , [Create (Ref "s")])]
    ]
  ]
