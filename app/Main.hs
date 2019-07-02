module Main where

import           Data.Either                    ( fromRight )
import           System.Directory               ( getAppUserDataDirectory )
import           System.Process                 ( ProcessHandle )

import           GitHub.Data.Definitions        ( SimpleUser(..) )
import           GitHub.Data.Id                 ( Id(..) )

import           Lib

appDataDir :: String
appDataDir = "spit"

currentUserId = 55533 -- 3433130  --

-- main :: IO ProcessHandle
main :: IO ()
main = do
  appUserDataDir <- getAppUserDataDirectory appDataDir
  let iconLocation = getIconLocation appUserDataDir
  -- let notification = Notification "Haskell" "Is Great"
  -- someFunc notification iconLocation
  eitherPRs <- getPRs
  prs <- case eitherPRs of
    Left err -> fail (show err)
    Right prs -> return prs
  let myPRs = extractAssignments currentUserId prs
      bodies = sequence $ extractBodies myPRs
  print bodies
  
