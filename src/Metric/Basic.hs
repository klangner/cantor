{- |
Module : Metric.Basic
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Basic software metrics.
-}
module Metric.Basic ( lineOfCode )where

import System.FilePath (takeExtension)
import qualified Data.ByteString as Str
import qualified Data.ByteString.Char8 as BS8
import Utils.Folder (listFilesR)


-- | Count number of lines in all files in given directory 
lineOfCode :: FilePath -> IO [(String, Int)] 
lineOfCode src = do 
    files <- listFilesR isSourceFile src
    let groups = groupByExt files
    mapM f groups
        where f (ext, xs) = do c <- countLines xs
                               return (ext, c)
                               
    
isSourceFile :: FilePath -> Bool
isSourceFile src = ext `elem` sourceExt
    where ext = takeExtension src
    
sourceExt :: [String]
sourceExt = [".java", ".hs", ".rb", ".js", ".py"]
    
groupByExt :: [FilePath] -> [(String, [FilePath])]
groupByExt xs = map f sourceExt
    where f :: String -> (String, [FilePath])
          f ext = (ext, filter (\y -> takeExtension y == ext) xs)

-- Count lines in file    
countLines :: [FilePath] -> IO Int
countLines xs = do
    lc <- mapM countFileLines xs
    return $ sum lc

countFileLines :: FilePath -> IO Int
countFileLines path = do
    contents <- Str.readFile path
    return $ length (BS8.lines contents)
    