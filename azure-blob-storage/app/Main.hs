module Main where

import qualified Blob (someFunc)

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    Blob.someFunc
