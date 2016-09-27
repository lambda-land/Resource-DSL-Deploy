module DSL.Test.Effect where

import Control.Exception.Base (catch)
import Test.Tasty
import Test.Tasty.HUnit

import DSL.Effect
import DSL.Environment
import DSL.Expression
import DSL.Path
import DSL.Primitive
import DSL.Resource


-- ** Helper functions

funEq :: Int -> Fun
funEq n = Fun (Param "x" TInt) (Ref "x" .== Lit (I n))

runEffect :: ResID -> Effect -> ResEnv -> IO ResEnv
runEffect rID eff env = snd <$> runInEmptyContext env (resolveEffect rID eff)

assertEffectError :: EffectErrorKind -> IO a -> Assertion
assertEffectError k act = (act >> failure "no error") `catch` \err ->
    if errorKind err == k
      then return ()
      else failure (show err)
  where
    failure got = assertFailure ("expected " ++ show k ++ " error, got: " ++ got)

testCases :: [Assertion] -> [TestTree]
testCases = zipWith (testCase . show) [1..]


-- ** Tests

testResolveEffect = testGroup "resolveEffect"
    [ testGroup "create" $ testCases
      
      [ do out <- runEffect "foo" (Create 3) envEmpty
           envFromList [("foo", I 3)] @=? out
      
      , do out <- runEffect "foo" (Create 3) envEmpty >>= runEffect "bar" (Create 4)
           envFromList [("foo", I 3), ("bar", I 4)] @=? out
      
      , do out <- runEffect "foo" (Create 3) envEmpty >>= runEffect "foo/bar" (Create 4)
           envFromList [("foo", I 3), ("foo/bar", I 4)] @=? out
      
      , assertEffectError ResourceAlreadyExists
          (runEffect "foo" (Create 3) envEmpty >>= runEffect "foo" (Create 4))
      ]

    , testGroup "delete" $ testCases

      [ do out <- runEffect "foo" Delete (envFromList [("foo", B True)])
           envEmpty @=? out
      
      , do out <- runEffect "foo" Delete (envFromList [("foo", B True), ("bar", Unit)])
           envFromList [("bar", Unit)] @=? out
      
      , do out <- runEffect "foo" Delete (envFromList [("foo", B True), ("foo/bar", Unit)])
           envFromList [("foo/bar", Unit)] @=? out
      
      , assertEffectError NoSuchResource
           (runEffect "foo" Delete envEmpty)
      
      , assertEffectError NoSuchResource
           (runEffect "foo" (Create 3) envEmpty
             >>= runEffect "foo" Delete
             >>= runEffect "foo" Delete)
      ]
    ]
