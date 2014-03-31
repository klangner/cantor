{- |
Module : Project.Types
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Project model
-}
module Project.Types ( Project(..) ) where

import Utils.Graph

-- | Project data
data Project = Project { projectPath :: FilePath
                       , projectPackages :: [String]
                       , projectGraph :: NamesGraph } deriving (Show) 

