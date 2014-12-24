{- |
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Analysis of language used in project.
-}
module Cantor.Analysis.Language ( countSourceFiles ) where

import System.FilePath (takeExtension)
import Data.List
import Cantor.Project (Project, projectFiles)
import Cantor.KnowledgeDB (KnowledgeDB, langFromExt)

-- | Count number of files for each language used in project
countSourceFiles :: KnowledgeDB -> Project -> [(String, Int)]
countSourceFiles es prj = map (\as -> (head as, length as)) ls3
    where ls1 = map ((langFromExt es) . takeExtension) (projectFiles prj)
          ls2 = filter (not . null) ls1
          ls3 = group (sort (map head ls2))
