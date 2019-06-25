module Lib
  ( someFunc
  , Notification (..)
  , getIconLocation
  )
where

-- Base/Prelude
import Text.Printf (printf)

-- 3rd Party
import System.Directory (getAppUserDataDirectory)
import System.Process (ProcessHandle, spawnCommand)

data Notification = Notification { title :: String
                                 , message :: String }
type IconLocation = String


getIconLocation :: FilePath -> String
getIconLocation appUserDataDir = appUserDataDir ++ "/github.png"

someFunc :: Notification -> IconLocation -> IO ProcessHandle
someFunc n iconLocation = spawnCommand (formatCommand n iconLocation)

formatCommand :: Notification -> IconLocation -> String
formatCommand n iconLocation = printf "notify-send -i %s \"%s\" \"%s\"" iconLocation (title n) (message n)

