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
import           System.Environment             ( lookupEnv )
import           Text.Printf                    ( printf )

-- 3rd Party
import           Data.Time.Clock                ( NominalDiffTime
                                                , UTCTime
                                                , diffUTCTime
                                                , getCurrentTime
                                                )
import qualified Data.Vector                   as V
import           GitHub.Auth                    ( Auth(OAuth) )
import           GitHub.Data.Definitions        ( Error
                                                , SimpleUser
                                                )
import           GitHub.Data.PullRequests       ( SimplePullRequest )
import qualified GitHub.Endpoints.PullRequests as PR
import           System.Directory               ( getAppUserDataDirectory )
import           System.Process                 ( ProcessHandle
                                                , spawnCommand
                                                )

data Notification = Notification { title :: String
                                 , message :: String }
type IconLocation = String

-- simplePullRequestRequestedReviewers
userId = "3433130"  -- mark-keaton

getIconLocation :: FilePath -> String
getIconLocation appUserDataDir = appUserDataDir ++ "/github.png"

someFunc :: Notification -> IconLocation -> IO ProcessHandle
someFunc n iconLocation = spawnCommand (formatCommand n iconLocation)

formatCommand :: Notification -> IconLocation -> String
formatCommand n iconLocation =
  printf "notify-send -i %s \"%s\" \"%s\"" iconLocation (title n) (message n)

getAuth :: IO (Maybe Auth)
getAuth = do
  pass <- lookupEnv "GITHUB_TOKEN"
  let passB = fmap B.pack pass
  let auth  = fmap OAuth passB
  return auth

testPR = do
  auth <- getAuth
  return $ PR.pullRequestsFor' auth "Tesorio" "Dashboard"

isAssigned :: SimpleUser -> SimplePullRequest -> Bool
isAssigned user pr =
  V.any (user ==) (PR.simplePullRequestRequestedReviewers pr)

extractAssignments
  :: SimpleUser -> V.Vector SimplePullRequest -> V.Vector SimplePullRequest
extractAssignments user = V.filter (isAssigned user)

isRecentPR :: NominalDiffTime -> UTCTime -> SimplePullRequest -> Bool
isRecentPR expectedDelta now pr = delta < expectedDelta
  where delta = diffUTCTime now (PR.simplePullRequestUpdatedAt pr)
