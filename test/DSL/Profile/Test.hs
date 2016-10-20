module DSL.Profile.Test where

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
funAddThree :: Fun
funAddThree = Fun (P "x" TInt) (Lit (I 3) + Ref "x")

ctx' :: Context
ctx' = Ctx ["foo"]
  (envFromList [("s", I 15), ("xx", I 10), ("y", I 1)])
  (envSingle "subprofile" (ProEntry (profile [P "p" TInt ,P "s" TInt]
                                     [(["foo"], [Create (Ref "xx")])])))
--

testProfile :: TestTree
testProfile = testGroup "Test Profile module"
  [ testGroup "Profile tests"
    [ testCase "profile func can create a simple profile" $
      do out <- return $
           profile [P "p" TInt ,P "s" TBool] [(["foo"], [Create (Ref "xx")])]
         p <- return $
           Profile [P "p" TInt ,P "s" TBool]
           (envFromList [(["foo"], [Create (Ref "xx")])])
         p @=? out

    , testCase "profile func can create a profile with more Effects" $
      do out <- return $
           profile [P "p" TInt ,P "s" TBool] [(["foo"], [Create (Ref "x")
                                                        , Check funAddThree
                                                        , Modify funAddThree])]
         p <- return $
           Profile [P "p" TInt ,P "s" TBool]
           (envFromList [(["foo"], [Create (Ref "x")
                                   , Check funAddThree
                                   , Modify funAddThree])])
         p @=? out
    ]

  , testGroup "loadProfile tests"
    [ testCase "loadProfile can load a profile" $
      do let p = profile [P "p" TInt ,P "ss" TInt] [(["bar"]
                                                    , [Create (Ref "s")])]
         let expr = [Ref "y", Lit (I 538)]
         let res = envFromList [(["foo"], Unit)]
         out <- snd <$> runInContext ctx' res (loadProfile p expr)
         t <- return $ envFromList [(["foo"], Unit), (["bar"], I 15)]
         t @=? out
    ]

  , testGroup "composeProfiles test"
    [ testCase "composeProfile can compose profiles" $
      do let p1 = profile [P "p" TInt ,P "ss" TInt] [(["bar"]
                                                     , [Create (Ref "s")])]
         let p2 = profile [P "x" TBool, P "a" TUnit] [(["foo"]
                                                     , [Check funAddThree])]
         out <- return $ composeProfiles p1 p2
         out @=? profile
           [P "p" TInt, P "ss" TInt, P "x" TBool, P "a" TUnit]
           [(["foo"], [Check funAddThree]), (["bar"], [Create (Ref "s")])]
    ]
  ]
