module DSL.Example.SwapDau.Test where

import Test.Tasty
import Test.Tasty.HUnit

import DSL.Environment
import DSL.Example.SwapDau
import DSL.Name
import DSL.Types


mkPort :: Name -> Name -> [(Name, PVal)] -> Port AttrVal
mkPort i f as = MkPort i f (MkPortAttrs (fmap Leaf (envFromList as)))

minimalOpts :: FilePath -> SwapOpts
minimalOpts req = defaultOpts
    { swapRulesFile = "json/test/swap-rules.json"
    , swapInventoryFile = "json/test/swap-inventory-minimal.json"
    , swapRequestFile = req
    }

minimalResponse :: Int -> Response
minimalResponse i = MkResponse
    [ MkResponseDau ["S1"]
      $ MkDau "I1"
        [ MkResponsePort "S1P1"
          $ mkPort "I1P1" "F1" [("Bar", I i)]
        ]
        10
    ]

twoPortOpts :: FilePath -> SwapOpts
twoPortOpts req = defaultOpts
    { swapRulesFile = "json/test/swap-rules.json"
    , swapInventoryFile = "json/test/swap-inventory-2ports.json"
    , swapRequestFile = req
    }

twoPortResponse :: Int -> Response
twoPortResponse i = MkResponse
    [ MkResponseDau ["S1"]
      $ MkDau "I1"
        [ MkResponsePort "S1P1"
          $ mkPort "I1P1" "F1" [("Bar", I i)]
        ]
        10
    ]

twoForOneOpts :: FilePath -> SwapOpts
twoForOneOpts req = defaultOpts
    { swapRulesFile = "json/test/swap-rules.json"
    , swapInventoryFile = "json/test/swap-inventory-2for1.json"
    , swapRequestFile = req
    }

twoForOneResponse1 :: Response
twoForOneResponse1 = MkResponse
    [ MkResponseDau ["S1"]
      $ MkDau "I2"
        [ MkResponsePort "S1P2"
          $ mkPort "I2P1" "F2" [("Bar", B True)]
        ]
        20
    , MkResponseDau ["S1"]
      $ MkDau "I1"
        [ MkResponsePort "S1P1"
          $ mkPort "I1P1" "F1" [("Foo", B True)]
        ]
        10
    ]

twoForOneResponse2 :: Response
twoForOneResponse2 = MkResponse
    [ MkResponseDau ["S1"]
      $ MkDau "I2"
        [ MkResponsePort "S1P1"
          $ mkPort "I2P1" "F2" [("Bar", B True)]
        ]
        20
    , MkResponseDau ["S1"]
      $ MkDau "I1"
        [ MkResponsePort "S1P2"
          $ mkPort "I1P1" "F1" [("Foo", B True)]
        ]
        10
    ]

twoForOneSameOpts :: SwapOpts
twoForOneSameOpts = defaultOpts
    { swapRulesFile = "json/test/swap-rules.json"
    , swapInventoryFile = "json/test/swap-inventory-2for1-same.json"
    , swapRequestFile = "json/test/swap-request-2for1-same.json"
    }

twoForOneSameResponse :: Response
twoForOneSameResponse = MkResponse
    [ MkResponseDau ["S1"]
      $ MkDau "I2"
        [ MkResponsePort "S1P2"
          $ mkPort "I2P1" "F1" [("Foo", B True)]
        ]
        20
    , MkResponseDau ["S1"]
      $ MkDau "I1"
        [ MkResponsePort "S1P1"
          $ mkPort "I1P1" "F1" [("Foo", B True)]
        ]
        10
    ]

groupOpts :: FilePath -> SwapOpts
groupOpts req = defaultOpts
    { swapRulesFile = "json/test/swap-rules.json"
    , swapInventoryFile = "json/test/swap-inventory-group.json"
    , swapRequestFile = req
    }

groupResponse1 :: Response
groupResponse1 = MkResponse
    [ MkResponseDau ["S1","S2"]
      $ MkDau "I1"
        [ MkResponsePort "S1-G1-P1"
          $ mkPort "I1-G1-P1" "F1" [("Foo", B True)]
        , MkResponsePort "S1-G1-P2"
          $ mkPort "I1-G1-P2" "F1" [("Foo", B True)]
        , MkResponsePort "S2-G1-P1"
          $ mkPort "I1-G1-P3" "F1" [("Foo", B True)]
        , MkResponsePort "S2-G2-P1"
          $ mkPort "I1-G2-P1" "F2" [("Foo", B True)]
        ]
        10
    ]

groupResponse3 :: Response
groupResponse3 = MkResponse
    [ MkResponseDau ["S1"]
      $ MkDau "I1"
        [ MkResponsePort "S1-G1-P1"
          $ mkPort "I1-G1-P1" "F1" [("Foo", B True)]
        , MkResponsePort "S1-G1-P2"
          $ mkPort "I1-G1-P2" "F1" [("Foo", B False)]
        ]
        10
    ]

rangeOpts :: FilePath -> SwapOpts
rangeOpts req = defaultOpts
    { swapRulesFile = "json/test/swap-rules.json"
    , swapInventoryFile = "json/test/swap-inventory-range.json"
    , swapRequestFile = req
    }

rangeResponse :: Int -> Int -> Response
rangeResponse i j = MkResponse
    [ MkResponseDau ["S1"]
      $ MkDau "I1"
        [ MkResponsePort "S1P2"
          $ mkPort "I1P1" "F1" [("Bar", I i)]
        , MkResponsePort "S1P1"
          $ mkPort "I1P2" "F1" [("Bar", I j)]
        ]
        10
    ]

equationOpts :: FilePath -> SwapOpts
equationOpts req = defaultOpts
    { swapRulesFile = "json/test/swap-rules.json"
    , swapInventoryFile = "json/test/swap-inventory-equation.json"
    , swapRequestFile = req
    }

equationResponse :: Name -> Int -> Int -> Name -> Int -> Int -> Response
equationResponse p1 s1 d1 p2 s2 d2 = MkResponse
    [ MkResponseDau ["S1"]
      $ MkDau "I1"
        [ MkResponsePort p1
          $ mkPort "I1P1" "F1"
            [ ("SampleRate", I s1)
            , ("DataLength", I d1)
            , ("DataRate", I (s1 * d1))
            ]
        , MkResponsePort p2
          $ mkPort "I1P2" "F1"
            [ ("SampleRate", I s2)
            , ("DataLength", I d2)
            , ("DataRate", I (s2 * d2))
            ]
        ]
        10
    ]

syncOpts :: FilePath -> SwapOpts
syncOpts req = defaultOpts
    { swapRulesFile = "json/test/swap-rules.json"
    , swapInventoryFile = "json/test/swap-inventory-sync.json"
    , swapRequestFile = req
    }

syncResponse :: Int -> Int -> Response
syncResponse s d = MkResponse
    [ MkResponseDau ["S1"]
      $ MkDau "I1"
        [ MkResponsePort "S1P1"
          $ MkPort "I1P1" "F1"
          $ MkPortAttrs $ envFromList
            [ ("Measurement", Node
              $ MkPortAttrs $ fmap Leaf $ envFromList
                [ ("SampleRate", I s)
                , ("DataLength", I d)
                , ("DataRate", I (s * d))
                ]
              )
            ]
        ]
        20
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
    , testGroup "two-for-one tests"
      [ testCase "1" $ do
          res <- runSwapTest (twoForOneOpts "json/test/swap-request-2for1-1.json")
          res @?= Just twoForOneResponse1
      , testCase "2" $ do
          res <- runSwapTest (twoForOneOpts "json/test/swap-request-2for1-2.json")
          res @?= Just twoForOneResponse2
      , testCase "3" $ do
          res <- runSwapTest (twoForOneOpts "json/test/swap-request-2for1-3.json")
          res @?= Nothing
      , testCase "4" $ do
          res <- runSwapTest twoForOneSameOpts
          res @?= Just twoForOneSameResponse
      ]
    , testGroup "group end-to-end tests"
      [ testCase "1" $ do
          res <- runSwapTest (groupOpts "json/test/swap-request-group1.json")
          res @?= Just groupResponse1
      , testCase "2" $ do
          res <- runSwapTest (groupOpts "json/test/swap-request-group2.json")
          res @?= Nothing
      , testCase "3" $ do
          res <- runSwapTest (groupOpts "json/test/swap-request-group3.json")
          res @?= Just groupResponse3
      ]
    , testGroup "range end-to-end tests"
      [ testCase "1" $ do
          res <- runSwapTest (rangeOpts "json/test/swap-request-range1.json")
          res @?= Just (rangeResponse 64 512)
      , testCase "2" $ do
          res <- runSwapTest (rangeOpts "json/test/swap-request-range2.json")
          res @?= Just (rangeResponse 128 1024)
      , testCase "3" $ do
          res <- runSwapTest (rangeOpts "json/test/swap-request-range3.json")
          res @?= Nothing
      ]
    , testGroup "equation end-to-end tests"
      [ testCase "1" $ do
          res <- runSwapTest (equationOpts "json/test/swap-request-equation1.json")
          res @?= Just (equationResponse "S1P2" 64 8 "S1P1" 1024 4)
      , testCase "2" $ do
          res <- runSwapTest (equationOpts "json/test/swap-request-equation2.json")
          res @?= Just (equationResponse "S1P1" 256 16 "S1P2" 768 2)
      ]
    , testGroup "sync end-to-end tests"
      [ testCase "1" $ do
          res <- runSwapTest (syncOpts "json/test/swap-request-sync1.json")
          res @?= Just (syncResponse 250 16)
      , testCase "2" $ do
          res <- runSwapTest (syncOpts "json/test/swap-request-sync2.json")
          res @?= Nothing
      ]
    ]
