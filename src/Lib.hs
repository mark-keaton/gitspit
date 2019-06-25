module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "hello world"

otherFunc :: IO ()
otherFunc = someFunc

