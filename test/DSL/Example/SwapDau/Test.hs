module DSL.Example.SwapDau.Test where

import Test.Tasty
import Test.Tasty.HUnit

import DSL.Environment
import DSL.Example.SwapDau
import DSL.Types


minimalOpts :: FilePath -> SwapOpts
minimalOpts req = defaultOpts
    { swapInventoryFile = "json/test/swap-inventory-minimal.json"
    , swapRequestFile = req
    }

minimalResponse :: Int -> Response
minimalResponse i = MkResponse
    [ MkResponseDau ["S1"]
      $ MkDau "I1"
        [ MkResponsePort "S1P1"
          $ MkPort "I1P1" "F1" (envFromList [("Bar", I i)])
        ]
        10
    ]

twoPortOpts :: FilePath -> SwapOpts
twoPortOpts req = defaultOpts
    { swapInventoryFile = "json/test/swap-inventory-2ports.json"
    , swapRequestFile = req
    }

twoPortResponse :: Int -> Response
twoPortResponse i = MkResponse
    [ MkResponseDau ["S1"]
      $ MkDau "I1"
        [ MkResponsePort "S1P1"
          $ MkPort "I1P1" "F1" (envFromList [("Bar", I i)])
        , MkResponsePort ""
          $ MkPort "I1P2" "F2" (envFromList [("Bar", I 4)])
        ]
        10
    ]

rangeOpts :: FilePath -> SwapOpts
rangeOpts req = defaultOpts
    { swapInventoryFile = "json/test/swap-inventory-range.json"
    , swapRequestFile = req
    }

rangeResponse :: Int -> Int -> Response
rangeResponse i j = MkResponse
    [ MkResponseDau ["S1"]
      $ MkDau "I1"
        [ MkResponsePort "S1P2"
          $ MkPort "I1P1" "F1" (envFromList [("Bar", I i)])
        , MkResponsePort "S1P1"
          $ MkPort "I1P2" "F1" (envFromList [("Bar", I j)])
        ]
        10
    ]

testSwap :: TestTree
testSwap =
  testGroup "Swap Tests"
    [ testGroup "minimal end-to-end tests"
      [ testCase "1" $ do
          res <- runSwapTest (minimalOpts "json/test/swap-request-minimal1.json")
          res @?= Nothing
      , testCase "2" $ do
          res <- runSwapTest (minimalOpts "json/test/swap-request-minimal2.json")
          res @?= Just (minimalResponse 2)
      , testCase "3" $ do
          res <- runSwapTest (minimalOpts "json/test/swap-request-minimal3.json")
          res @?= Just (minimalResponse 3)
      , testCase "4" $ do
          res <- runSwapTest (minimalOpts "json/test/swap-request-minimal4.json")
          res @?= Just (minimalResponse 4)
      , testCase "5" $ do
          res <- runSwapTest (minimalOpts "json/test/swap-request-minimal5.json")
          res @?= Nothing
      ]
    , testGroup "two-port end-to-end tests"
      [ testCase "1" $ do
          res <- runSwapTest (twoPortOpts "json/test/swap-request-minimal1.json")
          res @?= Nothing
      , testCase "2" $ do
          res <- runSwapTest (twoPortOpts "json/test/swap-request-minimal2.json")
          res @?= Just (twoPortResponse 2)
      , testCase "3" $ do
          res <- runSwapTest (twoPortOpts "json/test/swap-request-minimal3.json")
          res @?= Just (twoPortResponse 3)
      , testCase "4" $ do
          res <- runSwapTest (twoPortOpts "json/test/swap-request-minimal4.json")
          res @?= Just (twoPortResponse 4)
      , testCase "5" $ do
          res <- runSwapTest (twoPortOpts "json/test/swap-request-minimal5.json")
          res @?= Nothing
      ]
    , testGroup "range end-to-end tests"
      [ testCase "1" $ do
          res <- runSwapTest (rangeOpts "json/test/swap-request-range1.json")
          res @?= Just (rangeResponse 200 700)
      ]
    ]
