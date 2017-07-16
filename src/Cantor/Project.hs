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
                      , projectReqs
                      , scanProject ) where

import Data.List
import Data.Maybe (isJust)
import Cantor.Utils.Folder (listFilesR)
import System.FilePath (takeExtension)
import System.Directory
import Cantor.KnowledgeDB (KnowledgeDB, bsFromFilePath, langFromExt, reqFromKey)
import Cantor.Build.BuildSystem (BuildSystem, mkBuildSystem)
import qualified Cantor.Build.Maven as Maven
import qualified Cantor.Build.Cabal as Cabal


data Project = Prj { projectPath :: FilePath
                   , projectFiles :: [FilePath]             -- All project files
                   , projectLanguages :: [(String, Int)]    -- Language name and file count
                   , projectReqs :: [String]                -- Project requirements (eq. Haskell Platform)
                   , projectBuildSystem :: BuildSystem }

-- | Create new project by scanning all files at given path
scanProject :: KnowledgeDB -> FilePath -> IO Project
scanProject db path = do
    dp <- canonicalizePath path
    files <- listFilesR (const True) dp
    let n = length dp
    let fps = map (drop n) files
    let ls = countSourceFiles db fps
    bs <- readBS path (findBuildSystem db fps)
    let reqs = findRequirements db (map fst ls) bs
    return $ Prj path fps ls reqs bs

-- | Count number of files for each language used in project
countSourceFiles :: KnowledgeDB -> [FilePath] -> [(String, Int)]
countSourceFiles db fps = map (\as -> (head as, length as)) ls3
    where ls1 = map ((langFromExt db) . takeExtension) fps
          ls2 = filter (not . null) ls1
          ls3 = group (sort (map head ls2))

-- | Find build system used by project. Return build system name and path to build file.
findBuildSystem :: KnowledgeDB -> [FilePath] -> Maybe (FilePath, String)
findBuildSystem db fps = if null xs2 then Nothing else f(head xs2)
    where xs1 = map (\x -> (x, bsFromFilePath db x)) fps
          xs2 = filter (\(_,y) -> isJust y) xs1
          f (x, Just y) = Just (x,y)
          f (_, _) = Nothing

-- Read build system data
readBS :: FilePath -> Maybe (FilePath, String) -> IO BuildSystem
readBS path Nothing = return $ mkBuildSystem path "None" path
readBS path (Just (fp, "Maven")) = Maven.parseFile (path ++ fp)
readBS path (Just (fp, "Cabal")) = Cabal.parseFile (path ++ fp)
readBS path (Just (_, xs)) = return $ mkBuildSystem path xs path

-- Get requirements from build system
findRequirements :: KnowledgeDB -> [String] -> BuildSystem -> [String]
findRequirements db lang _ = concatMap (reqFromKey db) lang