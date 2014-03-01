module Main where

import System.Environment


main::IO()
main = do
    (path:_) <- getArgs
    putStrLn $ "Project path: " ++ path