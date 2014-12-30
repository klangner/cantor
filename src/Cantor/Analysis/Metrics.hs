{- |
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Analysis related to the language used in the project.
-}
module Cantor.Analysis.Metrics ( lineOfCode ) where


import System.FilePath (takeExtension)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Cantor.Project (Project, projectFiles, projectPath)
import Cantor.KnowledgeDB (KnowledgeDB, langFromExt)


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