{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc
  , Notification(..)
  , getIconLocation
  , getAuth
  , testPR
  )
where

-- Base/Prelude
import           Control.Applicative            ( (<*>) )
import qualified Data.ByteString.Char8         as B
import System.Environment (lookupEnv)
import           Text.Printf                    ( printf )

-- 3rd Party
import           GitHub.Auth                    ( Auth(OAuth) )
import qualified GitHub.Endpoints.PullRequests as PR
import           System.Directory               ( getAppUserDataDirectory )
import           System.Process                 ( ProcessHandle
                                                , spawnCommand
                                                )

data Notification = Notification { title :: String
                                 , message :: String }
type IconLocation = String


getIconLocation :: FilePath -> String
getIconLocation appUserDataDir = appUserDataDir ++ "/github.png"

someFunc :: Notification -> IconLocation -> IO ProcessHandle
someFunc n iconLocation = spawnCommand (formatCommand n iconLocation)

formatCommand :: Notification -> IconLocation -> String
formatCommand n iconLocation =
  printf "notify-send -i %s \"%s\" \"%s\"" iconLocation (title n) (message n)

getAuth = do
  -- user <- lookupEnv "GITHUB_USERNAME"
  pass <- lookupEnv "GITHUB_TOKEN"
  -- let userB = fmap B.pack user
  let passB = fmap B.pack pass
  let auth = fmap OAuth passB
  return auth

testPR = do
  auth <- getAuth
  prs <- PR.pullRequestsFor' auth "Tesorio" "Dashboard"
  print prs
