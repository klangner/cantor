{- |
Module : Cantor.Project
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Data Types and functions for procesing project
-}
module Cantor.Project ( Project
                      , projectPath
                      , projectFiles
                      , scanProject ) where

import Cantor.Utils.Folder (listFilesR)
import System.Directory


data Project = Prj { projectPath :: FilePath
                   , projectFiles :: [FilePath] } deriving(Show)

-- | Create new project by scanning all files at given path
scanProject :: FilePath -> IO Project
scanProject path = do
    dp <- canonicalizePath path
    files <- listFilesR (const True) dp
    let n = length dp
    let as = map (drop n) files
    return $ Prj path as
