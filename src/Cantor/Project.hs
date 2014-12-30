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
                      , countSourceFiles
                      , findBuildSystem
                      , projectPath
                      , projectFiles
                      , scanProject ) where

import Data.List
import Data.Maybe (isJust)
import Cantor.Utils.Folder (listFilesR)
import System.FilePath (takeExtension)
import System.Directory
import Cantor.KnowledgeDB (KnowledgeDB, bsFromFilePath, langFromExt)


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

-- | Count number of files for each language used in project
countSourceFiles :: KnowledgeDB -> Project -> [(String, Int)]
countSourceFiles db prj = map (\as -> (head as, length as)) ls3
    where ls1 = map ((langFromExt db) . takeExtension) (projectFiles prj)
          ls2 = filter (not . null) ls1
          ls3 = group (sort (map head ls2))

-- | Find build system used by project
findBuildSystem :: KnowledgeDB -> Project -> Maybe String
findBuildSystem db prj = if null xs2 then Nothing else head xs2
    where xs1 = map (bsFromFilePath db) (projectFiles prj)
          xs2 = filter (isJust) xs1
