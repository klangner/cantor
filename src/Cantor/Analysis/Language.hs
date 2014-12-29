{- |
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Analysis related to the language used in the project.
-}
module Cantor.Analysis.Language ( countSourceFiles
                                , lineOfCode ) where


import System.FilePath (takeExtension)
import Data.List
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Cantor.Project (Project, projectFiles, projectPath)
import Cantor.KnowledgeDB (KnowledgeDB, langFromExt)


-- | Count number of files for each language used in project
countSourceFiles :: KnowledgeDB -> Project -> [(String, Int)]
countSourceFiles db prj = map (\as -> (head as, length as)) ls3
    where ls1 = map ((langFromExt db) . takeExtension) (projectFiles prj)
          ls2 = filter (not . null) ls1
          ls3 = group (sort (map head ls2))


-- | Count number of lines in all files in given directory
lineOfCode :: KnowledgeDB -> Project -> IO Int
lineOfCode db prj = do
    let xs = projectFiles prj
    let fs = filter (not . null . (langFromExt db) . takeExtension) xs
    let fs2 = map ((projectPath prj) ++ ) fs
    ys <- mapM countFileLines  fs2
    return $ sum ys


-- Count lines in file
countFileLines :: FilePath -> IO Int
countFileLines path = do
    contents <- BS.readFile path
    return $ length (BS8.lines contents)