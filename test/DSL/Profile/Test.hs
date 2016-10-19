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

-- ** Helper functions s



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
    ]
  ]
