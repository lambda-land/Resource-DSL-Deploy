module DSL.Effect.Test where

import Control.Exception.Base (catch)
import Test.Tasty
import Test.Tasty.HUnit

import DSL.Effect
import DSL.Environment
import DSL.Expression
import DSL.Primitive
import DSL.Evaluation
import DSL.Path

-- ** Helper functions

funEq :: Int -> Fun
funEq n = Fun (Param "x" TInt) (Ref "x" .== Lit (I n))

funAddThree :: Fun
funAddThree = Fun (Param "x" TInt) (Lit (I 3) + Ref "x")

genFunAddThree :: Int -> Fun
genFunAddThree n = Fun (Param "x" TInt) (Lit (I 3) + Lit (I n))

runEffect :: ResID -> Effect -> ResEnv -> IO ResEnv
runEffect resid eff env = snd <$> runInEmptyContext env (resolveEffect resid eff)

assertEffectError :: EffectErrorKind -> IO a -> Assertion
assertEffectError k act = (act >> failure "no error") `catch` \err ->
    if effectErrorKind err == k
      then return ()
      else failure (show err)
  where
    failure got = assertFailure ("expected " ++ show k ++ " error, got: " ++ got)

assertArgTypeError :: IO a -> Assertion
assertArgTypeError act = (act >> failure "no error")
                         `catch` \(ArgTypeError _ _) -> return ()
  where
    failure got = assertFailure ("expected ArgTypeError" ++ " error, got: " ++ got)


testCases :: [Assertion] -> [TestTree]
testCases = zipWith (testCase . show) [1..]


-- ** Tests

testResolveEffect :: TestTree
testResolveEffect = testGroup "resolveEffect"
    [ testGroup "Create"

      [ testCase "Create with an empty Env" $
        do out <- runEffect (ResID ["foo"]) (Create 3) envEmpty
           envFromList [(ResID ["foo"], I 3)] @=? out

      , testCase "Create two Refs in ResEnv" $
        do out <- runEffect (ResID ["foo"]) (Create 3) envEmpty
                  >>= runEffect (ResID ["bar"]) (Create 4)
           envFromList [(ResID ["foo"], I 3), (ResID ["bar"], I 4)] @=? out

      , testCase "Create two Refs, /foo, and /foo/bar" $
        do out <- runEffect (ResID ["foo"]) (Create 3) envEmpty
                  >>= runEffect (ResID ["foo","bar"]) (Create 4)
           envFromList [(ResID ["foo"], I 3), (ResID ["foo","bar"], I 4)] @=? out

      , testCase "Create throws error if resource already exists" $
        assertEffectError
          ResourceAlreadyExists
          (runEffect (ResID ["foo"]) (Create 3) envEmpty
            >>= runEffect (ResID ["foo"]) (Create 4))
      ]

    , testGroup "delete"

      [ testCase "Delete deletes path at ResEnv" $
        do out <- runEffect (ResID ["foo"]) Delete
             (envFromList [(ResID ["foo"], B True)])
           envEmpty @=? out

      , testCase "Delete deletes at path with ResEnv with two Refs" $
        do out <- runEffect (ResID ["foo"])
             Delete (envFromList [(ResID ["foo"], B True)
                                 , (ResID ["bar"], Unit)])
           envFromList [(ResID ["bar"], Unit)] @=? out

      , testCase "Delete deletes foo when ResEnv has /foo and /foo/bar" $
        do out <- runEffect (ResID ["foo"]) Delete
             (envFromList [(ResID ["foo"], B True), (ResID ["foo","bar"], Unit)])
           envFromList [(ResID ["foo","bar"], Unit)] @=? out

      , testCase "Delete throws NoSuchResource on empty Env" $
        assertEffectError NoSuchResource
           (runEffect (ResID ["foo"]) Delete envEmpty)

      , testCase "Delete throws NoSuchResource when deleting foo twice" $
        assertEffectError NoSuchResource
           (runEffect (ResID ["foo"]) (Create 3) envEmpty
             >>= runEffect (ResID ["foo"]) Delete
             >>= runEffect (ResID ["foo"]) Delete)
      ]

    , testGroup "Check"

      [ testCase "Check a path exists in ResEnv" $
        do out <- runEffect (ResID ["foo"]) (Check (funEq 1729)) 
             (envFromList [(ResID ["foo"], I 1729)]) 
           envFromList [(ResID ["foo"], I 1729)] @=? out

      , testCase "Check a path exists in ResEnv with two paths" $
        do out <- runEffect (ResID ["foo"]) (Check (funEq 1729)) 
             (envFromList [(ResID ["foo"], I 1729), (ResID ["bar"], I 2)]) 
           envFromList [(ResID ["foo"], I 1729), (ResID ["bar"], I 2)] @=? out

      , testCase "Check a created path" $
        do out <- runEffect (ResID ["foo"]) (Create 3) envEmpty
             >>= runEffect (ResID ["foo"]) (Check (funEq 3)) 
           envFromList [(ResID ["foo"], I 3)] @=? out

      , testCase "Check a created path and return a False" $
        assertEffectError
          CheckFailure (runEffect (ResID ["foo"])
                        (Create 3) envEmpty
                        >>= runEffect (ResID ["foo"]) (Check (funEq 1)))

      , testCase "Check created path, throw type error" $
        assertArgTypeError (runEffect (ResID ["foo"]) (Check (funEq 3))
                          (envFromList [(ResID ["foo"], B True)]))

      , testCase "Check fails to check /foo/bar when checking /foo" $
        assertEffectError
          NoSuchResource (runEffect (ResID ["foo"])
                          (Check (funEq 1729))
                          (envFromList [(ResID ["foo", "bar"], I 1729)]))

      , testCase "Check fails to check with empty ResEnv" $
          assertEffectError NoSuchResource (runEffect (ResID ["foo"])
                                          (Check (funEq 1729)) envEmpty)
      ]

      , testGroup "Modify"

      [ testCase "Modify can modify in ResEnv with a Ref in VarEnv" $
          do out <- runEffect (ResID ["foo"]) (Modify funAddThree)
               (envFromList [(ResID ["foo"], I 0)])
             envFromList [(ResID ["foo"], I 3)] @=? out

      , testCase "Modify a created path" $
          do
            out <- runEffect (ResID ["foo"]) (Create 3) envEmpty
                   >>= runEffect (ResID ["foo"]) (Modify  funAddThree)
            envFromList [(ResID ["foo"], I 6)] @=? out

      , testCase "Modify, modifies in ResEnv with two Refs" $
        do
          out <- runEffect (ResID ["foo"])
             (Modify funAddThree)
             (envFromList [(ResID ["foo"], I 3), (ResID ["bar"], I 1729)])
          (envFromList [(ResID ["foo"], I 6), (ResID ["bar"], I 1729)]) @=? out

      , testCase "Modify can modify in env with two Refs, twice" $
        do out <- runEffect (ResID ["foo"]) (Modify funAddThree)
             (envFromList [(ResID ["foo"], I 3), (ResID ["bar"], I 1729)])
                  >>= runEffect (ResID ["bar"]) (Modify funAddThree)
           envFromList [(ResID ["foo"], I 6), (ResID ["bar"], I 1732)] @=? out

      , testCase "Throw an ArgTypeError when trying to mod a boolean with an int" $
        assertArgTypeError (runEffect (ResID ["foo"])
                            (Modify funAddThree)
                            (envFromList [(ResID ["foo"], B True)]))

      , testCase "Modify fails to modify on Evn with path foo/bar" $
        assertEffectError
          NoSuchResource (runEffect (ResID ["foo"])
                          (Modify funAddThree)
                          (envFromList [(ResID ["foo", "bar"], I 1729)]))

      , testCase "Modify fails to modify on an empty ResEnv" $
        assertEffectError
          NoSuchResource (runEffect (ResID ["foo"])
                          (Modify funAddThree)
                          envEmpty)
      ]
    ]
