
module Example.Location where

import DSL.Environment
import DSL.Syntax
import DSL.Type


-- | Use built-in android GPS API.
gpsAndroidT :: RType
gpsAndroidT = RT [] henvEmpty
