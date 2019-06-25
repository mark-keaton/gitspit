module Main where

import System.Directory (getAppUserDataDirectory)
import System.Process (ProcessHandle)

import Lib

appDataDir :: String
appDataDir = "spit"

main :: IO ProcessHandle
main = do
  appUserDataDir <- getAppUserDataDirectory appDataDir
  let iconLocation = getIconLocation appUserDataDir
  let notification = Notification "Haskell" "Is Great"
  someFunc notification iconLocation
