module Example.Location where

import Data.List (intercalate,subsequences)

import DSL.Expr
import DSL.Type
import DSL.Predicate
import DSL.Primitive
import DSL.Row


--
-- * Location Provider Challenge Problem
--


-- ** DFUs

-- | A list of all the location DFUs with associated names.
locationDFUs :: [(String, Expr Refined)]
locationDFUs =
    [ ("gps-android", gpsAndroid)
    , ("gps-bluetooth", gpsBluetooth)
    , ("gps-usb", gpsUsb)
    , ("gps-saasm", gpsSaasm)
    , ("dead-reckoning", deadReck)
    ]

-- | Use built-in android GPS API.
gpsAndroid :: Expr Refined
gpsAndroid = Fun "r" gpsAndroidT
    $ Ext "Location" (Free Unit)
    $ recCheck "GPS-Sat"
    $ recCheck "GPS-Dev"
    $ Use "r"

-- | Bluetooth based GPS.
gpsBluetooth :: Expr Refined
gpsBluetooth = Fun "r" gpsBluetoothT
    $ Ext "Location" (Free Unit)
    $ recCheck "GPS-Sat"
    $ recCheck "Ext-BT"
    $ Use "r"

-- | Generic USB-based GPS.
gpsUsb :: Expr Refined
gpsUsb = Fun "r" gpsUsbT
    $ Ext "Location" (Free Unit)
    $ recCheck "GPS-Sat"
    $ recCheck "Ext-USB"
    $ Use "r"

-- | USB-based SAASM GPS.
gpsSaasm :: Expr Refined
gpsSaasm = Fun "r" gpsSaasmT
    $ Ext "Location" (Free Unit)
    $ Ext "SAASM-Location" (Free Unit)
    $ recCheck "GPS-Sat"
    $ recCheck "Ext-USB"
    $ Use "r"

-- | Manual / dead reckoning location capability.
deadReck :: Expr Refined
deadReck = Fun "r" deadReckT
    $ Ext "Location" (Free Unit)
    $ recCheck "Has-UI"
    $ Use "r"


-- ** DFU types (eventually, these can be inferred)

-- | Creates a record entry for a reusable atomic resource.
has :: Label -> (Label, Type Refined)
has l = (l, Bang tUnit)

-- | Type of gpsAndroid.
gpsAndroidT :: Schema Refined
gpsAndroidT = Forall ["r"]
    $ polyRec "r" [has "GPS-Sat", has "GPS-Dev"]
  :-> polyRec "r" [has "GPS-Sat", has "GPS-Dev", has "Location"]

-- | Type of gpsBluetooth.
gpsBluetoothT :: Schema Refined
gpsBluetoothT = Forall ["r"]
    $ polyRec "r" [has "GPS-Sat", has "Ext-BT"]
  :-> polyRec "r" [has "GPS-Sat", has "Ext-BT", has "Location"]

-- | Type of gpsUsb.
gpsUsbT :: Schema Refined
gpsUsbT = Forall ["r"]
    $ polyRec "r" [has "GPS-Sat", has "Ext-USB"]
  :-> polyRec "r" [has "GPS-Sat", has "Ext-USB", has "Location"]

-- | Type of gpsSaasm.
gpsSaasmT :: Schema Refined
gpsSaasmT = Forall ["r"]
    $ polyRec "r" [has "GPS-Sat", has "Ext-USB"]
  :-> polyRec "r" [has "GPS-Sat", has "Ext-USB", has "Location", has "SAASM-Location"]

-- | Type of deadReck.
deadReckT :: Schema Refined
deadReckT = Forall ["r"]
    $ polyRec "r" [has "Has-UI"]
  :-> polyRec "r" [has "Has-UI", has "Location"]


-- ** Environments, requirements, application model

-- | Generate an initial resource environment with the corresponding
--   capabilities.
initEnv :: [Label] -> Expr Refined
initEnv = rec . map (\l -> (l, Free Unit))

-- | Generate all possible initial resource environments for a given list
--   of capabilities; pair each one with a corresponding name.
initEnvs :: [Label] -> [(String, Expr Refined)]
initEnvs = map (\ls -> (intercalate "+" ls, initEnv ls)) . tail . subsequences

-- | All possible initial environments for the location scenario.
allEnvs :: [(String, Expr Refined)]
allEnvs = initEnvs ["GPS-SAT", "GPS-Dev", "Ext-BT", "Ext-USB", "Has-UI"]

-- | Indicates that a function is untyped (temporary solution).
untyped :: Schema Refined
untyped = Forall [] tUnit

-- | Trivial application model.
appModel :: Expr Refined
appModel = Fun "dfu" untyped (Fun "env" untyped (App (Use "dfu") (Use "env")))
