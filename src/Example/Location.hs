module Example.Location where

import DSL.Model
import DSL.Profile
import DSL.Resource


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
      [ checkPresent "SAT"
      , checkPresent "Dev" ]
    , provideUnit "Location"
    ]

-- | Bluetooth-based GPS.
gpsBluetooth :: Model
gpsBluetooth = Model []
    [ In ["GPS"] [checkPresent "SAT"]
    , In ["Ext"] [checkPresent "BT"]
    , provideUnit "Location"
    ]

-- | Generic USB-based GPS.
gpsUsb :: Model
gpsUsb = Model []
    [ In ["GPS"] [checkPresent "SAT"]
    , In ["Ext"] [checkPresent "USB"]
    , provideUnit "Location"
    ]

-- | USB-based SAASM GPS.
gpsSaasm :: Model
gpsSaasm = Model []
    [ In ["GPS"] [checkPresent "SAT"]
    , In ["Ext"] [checkPresent "USB"]
    , provideUnit "Location"
    , In ["Location"] [provideUnit "SAASM"]
    ]

-- | Manual / dead reckoning location capability.
deadReck :: Model
deadReck = Model []
    [ checkPresent "UI"
    , provideUnit "Location"
    ]


-- ** Application model

-- | Trivial application model.
