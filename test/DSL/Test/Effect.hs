module DSL.Test.Effect where

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

testCases :: [Assertion] -> [TestTree]
testCases = zipWith (testCase . show) [1..]


-- ** Tests

testResolveEffect :: TestTree
testResolveEffect = testGroup "resolveEffect"
    [ testGroup "Create" $ testCases
      
      [ do out <- runEffect ["foo"] (Create 3) envEmpty
           envFromList [(["foo"], I 3)] @=? out
      
      , do out <- runEffect ["foo"]
             (Create 3) envEmpty >>= runEffect ["bar"] (Create 4)
           envFromList [(["foo"], I 3), (["bar"], I 4)] @=? out
      
      , do out <- runEffect ["foo"]
             (Create 3) envEmpty >>= runEffect ["foo","bar"] (Create 4)
           envFromList [(["foo"], I 3), (["foo","bar"], I 4)] @=? out
      
      , assertEffectError
          ResourceAlreadyExists
          (runEffect ["foo"] (Create 3) envEmpty
            >>= runEffect ["foo"] (Create 4))
      ]

    , testGroup "delete" $ testCases

      [ do out <- runEffect ["foo"] Delete (envFromList [(["foo"], B True)])
           envEmpty @=? out
      
      , do out <- runEffect ["foo"]
             Delete (envFromList [(["foo"], B True), (["bar"], Unit)])
           envFromList [(["bar"], Unit)] @=? out
      
      , do out <- runEffect ["foo"]
             Delete (envFromList [(["foo"], B True), (["foo","bar"], Unit)])
           envFromList [(["foo","bar"], Unit)] @=? out
      
      , assertEffectError NoSuchResource
           (runEffect ["foo"] Delete envEmpty)
      
      , assertEffectError NoSuchResource
           (runEffect ["foo"] (Create 3) envEmpty
             >>= runEffect ["foo"] Delete
             >>= runEffect ["foo"] Delete)
      ]

    , testGroup "Check" $ testCases

      -- check that check Checks a path exists in Resource Env
      [ do out <- runEffect ["foo"] (Check (funEq 1729)) 
             (envFromList [(["foo"], I 1729)]) 
           envFromList [(["foo"], I 1729)] @=? out

      -- check that check, checks a path exists in ResEnv with two paths
      , do out <- runEffect ["foo"] (Check (funEq 1729)) 
             (envFromList [(["foo"], I 1729), (["bar"], I 2)]) 
           envFromList [(["foo"], I 1729), (["bar"], I 2)] @=? out

      -- check that check, checks a path that is created by running Create
      , do out <- runEffect ["foo"] (Create 3) envEmpty
             >>= runEffect ["foo"] (Check (funEq 3)) 
           envFromList [(["foo"], I 3)] @=? out
           
      -- check that check, checks a path that is created and return a False
      , assertEffectError
          CheckFailure (runEffect ["foo"]
                        (Create 3) envEmpty
                        >>= runEffect ["foo"] (Check (funEq 1)))
                        
           
      -- check that check doesn't find a mismatched path in ResEnv
      , assertEffectError
          NoSuchResource (runEffect ["foo"]
                          (Check (funEq 1729))
                          (envFromList [(["foo", "bar"], I 1729)]))

      -- check that check fails to find a path in an empty ResEnv
      , assertEffectError NoSuchResource (runEffect ["foo"]
                                          (Check (funEq 1729)) envEmpty)
      ]

      , testGroup "Modify" $ testCases

      -- check that Modify can modify in ResEnv with a Ref in VarEnv
      [ do out <- runEffect ["foo"] (Modify funAddThree)
             (envFromList [(["foo"], I 0)])
           envFromList [(["foo"], I 3)] @=? out

      -- check that Modify can modify in env with two Refs
      , do out <- runEffect ["foo"] (Modify funAddThree)
             (envFromList [(["foo"], I 3), (["bar"], I 1729)])
           envFromList [(["foo"], I 6), (["bar"], I 1729)] @=? out

      -- check that Modify can modify in env with two Refs, twice
      , do out <- runEffect ["foo"] (Modify funAddThree)
             (envFromList [(["foo"], I 3), (["bar"], I 1729)])
               >>= runEffect ["foo"] (Modify funAddThree)
           envFromList [(["foo"], I 9), (["bar"], I 1729)] @=? out
      
      -- check that Modify fails to modify on evn with list Ref
      , assertEffectError
          NoSuchResource (runEffect ["foo"]
                          (Modify funAddThree)
                          (envFromList [(["foo", "bar"], I 1729)]))

      -- check that Modify fails on empty evn
      -- check that Modify can apply a Fun to an env ref    

      ]
    ]
