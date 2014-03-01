{- |
Module : Utils.Folder
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Helper module with functions operating on IO
-}
module Utils.Folder
        ( listFiles
        , listDirs
        )where

import System.Directory (canonicalizePath, getDirectoryContents, doesDirectoryExist)
import Data.List
import Control.Monad


-- | list files
listFiles :: FilePath -> IO [FilePath]            
listFiles p = do 
    contents <- list p
    filterM (fmap not . doesDirectoryExist) contents

    
-- | list subdirectories
listDirs :: FilePath -> IO [FilePath]            
listDirs p = do
    contents <- list p
    filterM doesDirectoryExist contents
    
-- | list directory content
list :: FilePath -> IO [FilePath]            
list p = do 
    ds <- getDirectoryContents p
    let filtered = filter f ds
    path <- canonicalizePath p  
    return $ map ((path++"/")++) filtered
    where f x = (x /= ".") && (x /= "..") && (not . isPrefixOf ".") x
    