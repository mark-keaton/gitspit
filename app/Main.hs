module Main where

import System.Process (ProcessHandle)

import Lib

main :: IO ProcessHandle
main = do
  let notification = Notification "Haskell" "Is Great"
  someFunc notification
