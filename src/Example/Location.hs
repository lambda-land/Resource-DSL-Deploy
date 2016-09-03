module Example.Location where

import Data.List (intercalate,subsequences)

import DSL.Environment
import DSL.Expression
import DSL.Model
import DSL.Primitive
import DSL.Profile
import DSL.Resource


-- ** Application model

-- | Location-provider application model. The input parameter selects which
--   location provider to use. NOTE: This is the only thing that is currently
--   required to be written in the DSL directly. Everything else can be passed
--   in via the JSON interface.
appModel = Model [("provider", TInt)]
    [ caseOf (Ref "provider")
      [ (1, [Load "gps-android" []])
      , (2, [Load "gps-bluetooth" []])
      , (3, [Load "gps-usb" []])
      , (4, [Load "gps-saasm" []])
      ] [Load "dead-reckoning" []]
    ]


-- ** DFUs

-- | A dictionary of all the location DFUs with associated names.
locationDFUs :: Dictionary
locationDFUs = profileDict $
    [ ("gps-android",    gpsAndroid)
    , ("gps-bluetooth",  gpsBluetooth)
    , ("gps-usb",        gpsUsb)
    , ("gps-saasm",      gpsSaasm)
    , ("dead-reckoning", deadReck)
    ]

-- | Use built-in android GPS API.
gpsAndroid :: Model
gpsAndroid = Model []
    [ In ["GPS"]
      [ checkUnit "SAT"
      , checkUnit "Dev" ]
    , provideUnit "Location"
    ]

-- | Bluetooth-based GPS.
gpsBluetooth :: Model
gpsBluetooth = Model []
    [ In ["GPS"] [checkUnit "SAT"]
    , In ["Ext"] [checkUnit "BT"]
    , provideUnit "Location"
    ]

-- | Generic USB-based GPS.
gpsUsb :: Model
gpsUsb = Model []
    [ In ["GPS"] [checkUnit "SAT"]
    , In ["Ext"] [checkUnit "USB"]
    , provideUnit "Location"
    ]

-- | USB-based SAASM GPS.
gpsSaasm :: Model
gpsSaasm = Model []
    [ In ["GPS"] [checkUnit "SAT"]
    , In ["Ext"] [checkUnit "USB"]
    , provideUnit "Location"
    , In ["Location"] [provideUnit "SAASM"]
    ]

-- | Manual / dead reckoning location capability.
deadReck :: Model
deadReck = Model []
    [ checkUnit "UI"
    , provideUnit "Location"
    ]


-- ** Initial environments

-- | All relevant initial environments for the location scenario.
locationEnvs :: [(String, ResEnv)]
locationEnvs = [(toID ps, toEnv ps) | ps <- tail (subsequences paths)]
  where
    toEnv = envFromList . map (\p -> (p,Unit))
    toID  = intercalate "+" . map (intercalate ".")
    paths = [["GPS","SAT"],["GPS","Dev"],["Ext","USB"],["Ext","BT"],["UI"]]


-- ** Mission requirements

-- | Require location.
hasLocation :: Profile
hasLocation = toProfile $ Model [] [checkUnit "Location"]

-- | Require SAASM location.
hasSaasm :: Profile
hasSaasm = toProfile $ Model []
    [ checkUnit "Location"
    , In ["Location"] [checkUnit "SAASM"]
    ]

-- | All relevant mission requirements for the location scenario.
locationReqs :: [(String, Profile)]
locationReqs = [("location", hasLocation), ("saasm", hasSaasm)]
