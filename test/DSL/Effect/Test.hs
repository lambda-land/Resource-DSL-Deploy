module DSL.Effect.Test where

import Control.Exception.Base (catch)
import Test.Tasty
import Test.Tasty.HUnit

import DSL.Effect
import DSL.Environment
import DSL.Expression
import DSL.Primitive
import DSL.Resource


-- ** Helper functions

funEq :: Int -> Fun
funEq n = Fun (P "x" TInt) (Ref "x" .== Lit (I n))

funAddThree :: Fun
funAddThree = Fun (P "x" TInt) (Lit (I 3) + Ref "x")

genFunAddThree :: Int -> Fun
genFunAddThree n = Fun (P "x" TInt) (Lit (I 3) + Lit (I n))

runEffect :: Path -> Effect -> ResEnv -> IO ResEnv
runEffect path eff env = snd <$> runInEmptyContext env (resolveEffect path eff)

assertEffectError :: EffectErrorKind -> IO a -> Assertion
assertEffectError k act = (act >> failure "no error") `catch` \err ->
    if errorKind err == k
      then return ()
      else failure (show err)
  where
    failure got = assertFailure ("expected " ++ show k ++ " error, got: " ++ got)

assertArgTypeError :: IO a -> Assertion
assertArgTypeError act = (act >> failure "no error") `catch` \err ->
  if isArgTypeError err == True
     then return ()
     else failure (show err)
  where
    failure got = assertFailure ("Expected argType error " ++ "got: " ++ got)

testCases :: [Assertion] -> [TestTree]
testCases = zipWith (testCase . show) [1..]


-- ** Tests

testResolveEffect :: TestTree
testResolveEffect = testGroup "resolveEffect"
    [ testGroup "Create" 
      
      [ testCase "Create with an empty Env" $
        do out <- runEffect ["foo"] (Create 3) envEmpty
           envFromList [(["foo"], I 3)] @=? out
      
      , testCase "Create two Refs in ResEnv" $
        do out <- runEffect ["foo"] (Create 3) envEmpty >>= runEffect ["bar"] (Create 4)
           envFromList [(["foo"], I 3), (["bar"], I 4)] @=? out
      
      , testCase "Create two Refs, /foo, and /foo/bar" $
        do out <- runEffect ["foo"] (Create 3) envEmpty >>= runEffect ["foo","bar"] (Create 4)
           envFromList [(["foo"], I 3), (["foo","bar"], I 4)] @=? out
      
      , testCase "Create throws error if resource already exists" $
        assertEffectError
          ResourceAlreadyExists
          (runEffect ["foo"] (Create 3) envEmpty
            >>= runEffect ["foo"] (Create 4))
      ]

    , testGroup "delete"

      [ testCase "Delete deletes path at ResEnv" $
        do out <- runEffect ["foo"] Delete (envFromList [(["foo"], B True)])
           envEmpty @=? out
      
      , testCase "Delete deletes at path with ResEnv with two Refs" $
        do out <- runEffect ["foo"]
             Delete (envFromList [(["foo"], B True), (["bar"], Unit)])
           envFromList [(["bar"], Unit)] @=? out
      
      , testCase "Delete deletes foo when ResEnv has /foo and /foo/bar" $
        do out <- runEffect ["foo"] Delete (envFromList [(["foo"], B True), (["foo","bar"], Unit)])
           envFromList [(["foo","bar"], Unit)] @=? out
      
      , testCase "Delete throws NoSuchResource on empty Env" $
        assertEffectError NoSuchResource
           (runEffect ["foo"] Delete envEmpty)
      
      , testCase "Delete throws NoSuchResource when deleting foo twice" $
        assertEffectError NoSuchResource
           (runEffect ["foo"] (Create 3) envEmpty
             >>= runEffect ["foo"] Delete
             >>= runEffect ["foo"] Delete)
      ]

    , testGroup "Check"

      [ testCase "Check a path exists in ResEnv" $
        do out <- runEffect ["foo"] (Check (funEq 1729)) 
             (envFromList [(["foo"], I 1729)]) 
           envFromList [(["foo"], I 1729)] @=? out

      , testCase "Check a path exists in ResEnv with two paths" $
        do out <- runEffect ["foo"] (Check (funEq 1729)) 
             (envFromList [(["foo"], I 1729), (["bar"], I 2)]) 
           envFromList [(["foo"], I 1729), (["bar"], I 2)] @=? out

      , testCase "Check a created path" $
        do out <- runEffect ["foo"] (Create 3) envEmpty
             >>= runEffect ["foo"] (Check (funEq 3)) 
           envFromList [(["foo"], I 3)] @=? out
           
      , testCase "Check a created path and return a False" $
        assertEffectError
          CheckFailure (runEffect ["foo"]
                        (Create 3) envEmpty
                        >>= runEffect ["foo"] (Check (funEq 1)))
                        
      , testCase "Check created path, throw type error" $
        assertArgTypeError (runEffect ["foo"] (Check (funEq 3))
                          (envFromList [(["foo"], B True)]))

      , testCase "Check fails to check /foo/bar when checking /foo" $
        assertEffectError
          NoSuchResource (runEffect ["foo"]
                          (Check (funEq 1729))
                          (envFromList [(["foo", "bar"], I 1729)]))

      , testCase "Check fails to check with empty ResEnv" $
          assertEffectError NoSuchResource (runEffect ["foo"]
                                          (Check (funEq 1729)) envEmpty)
      ]

      , testGroup "Modify"

      [ testCase "Modify can modify in ResEnv with a Ref in VarEnv" $
          do out <- runEffect ["foo"] (Modify funAddThree) (envFromList [(["foo"], I 0)])
             envFromList [(["foo"], I 3)] @=? out

      , testCase "Modify a created path" $
          do
            out <- runEffect ["foo"] (Create 3) envEmpty >>= runEffect ["foo"] (Modify  funAddThree)
            envFromList [(["foo"], I 6)] @=? out

      , testCase "Modify, modifies in ResEnv with two Refs" $
        do
          out <- runEffect ["foo"]
             (Modify funAddThree)
             (envFromList [(["foo"], I 3), (["bar"], I 1729)])
          (envFromList [(["foo"], I 6), (["bar"], I 1729)]) @=? out

      , testCase "Modify can modify in env with two Refs, twice" $
        do out <- runEffect ["foo"] (Modify funAddThree) (envFromList [(["foo"], I 3), (["bar"], I 1729)]) >>= runEffect ["bar"] (Modify funAddThree)
           envFromList [(["foo"], I 6), (["bar"], I 1732)] @=? out
      
      , testCase "Throw an ArgTypeError when trying to mod a boolean with an int" $
        assertArgTypeError (runEffect ["foo"]
                            (Modify funAddThree)
                            (envFromList [(["foo"], B True)]))

      , testCase "Modify fails to modify on Evn with path foo/bar" $
        assertEffectError
          NoSuchResource (runEffect ["foo"]
                          (Modify funAddThree)
                          (envFromList [(["foo", "bar"], I 1729)]))

      , testCase "Modify fails to modify on an empty ResEnv" $
        assertEffectError
          NoSuchResource (runEffect ["foo"]
                          (Modify funAddThree)
                          envEmpty)
        

      ]
    ]
