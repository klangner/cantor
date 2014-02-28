module Main where

import System.Environment


main::IO()
main = do
    (path:_) <- getArgs
    putStrLn $ "Command: " ++ path