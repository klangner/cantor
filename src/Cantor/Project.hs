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
                      , projectBuildSystem
                      , projectPath
                      , projectFiles
                      , projectLanguages
                      , scanProject ) where

import Data.List
import Data.Maybe (isJust)
import Cantor.Utils.Folder (listFilesR)
import System.FilePath (takeExtension)
import System.Directory
import Cantor.KnowledgeDB (KnowledgeDB, bsFromFilePath, langFromExt)


data Project = Prj { projectPath :: FilePath
                   , projectFiles :: [FilePath]
                   , projectLanguages :: [(String, Int)]  -- Language name and file count
                   , projectBuildSystem :: Maybe String }

-- | Create new project by scanning all files at given path
scanProject :: KnowledgeDB -> FilePath -> IO Project
scanProject db path = do
    dp <- canonicalizePath path
    files <- listFilesR (const True) dp
    let n = length dp
    let fps = map (drop n) files
    let ls = countSourceFiles db fps
    let bs = findBuildSystem db fps
    return $ Prj path fps ls bs

-- | Count number of files for each language used in project
countSourceFiles :: KnowledgeDB -> [FilePath] -> [(String, Int)]
countSourceFiles db fps = map (\as -> (head as, length as)) ls3
    where ls1 = map ((langFromExt db) . takeExtension) fps
          ls2 = filter (not . null) ls1
          ls3 = group (sort (map head ls2))

-- | Find build system used by project
findBuildSystem :: KnowledgeDB -> [FilePath] -> Maybe String
findBuildSystem db fps = if null xs2 then Nothing else head xs2
    where xs1 = map (bsFromFilePath db) fps
          xs2 = filter (isJust) xs1
