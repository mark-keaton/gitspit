module Lib
  ( someFunc
  , Notification (..)
  )
where

-- Base/Prelude
import Text.Printf (printf)

-- 3rd Party
import System.Process (ProcessHandle, spawnCommand)

data Notification = Notification { title :: String
                                 , message :: String }

someFunc :: Notification -> IO ProcessHandle
someFunc n = spawnCommand (formatCommand n)

formatCommand :: Notification -> String
formatCommand n = printf "notify-send \"%s\" \"%s\"" (title n) (message n)

