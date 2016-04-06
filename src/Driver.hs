module Driver where

import System.Environment (getArgs,withArgs)

import Example.DemoDriver
import Example.LocationDriver

runDriver :: IO ()
runDriver = do
  args <- getArgs
  case args of
    "location" : as -> withArgs as locationDriver
    _ -> demoDriver
