module Main where

import           Data.Either                    ( fromRight )
import           System.Directory               ( getAppUserDataDirectory )
import           System.Process                 ( ProcessHandle )

import           GitHub.Data.Definitions        ( SimpleUser(..) )
import           GitHub.Data.Id                 ( Id(..) )

import           Lib

appDataDir :: String
appDataDir = "spit"

currentUserId = 3433130  -- 55533

-- main :: IO ProcessHandle
main :: IO ()
main = do
  appUserDataDir <- getAppUserDataDirectory appDataDir
  let iconLocation = getIconLocation appUserDataDir
  -- let notification = Notification "Haskell" "Is Great"
  -- someFunc notification iconLocation
  eitherPRs <- getPRs
  let prs = fromRight (error "") eitherPRs
      myPRs = extractAssignments currentUserId prs
      bodies = sequence $ extractBodies myPRs
  print bodies
  
