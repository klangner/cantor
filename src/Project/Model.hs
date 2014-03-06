{- |
Module : Project.Model
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Project model
-}
module Project.Model ( Metadata(..)
                     , Project(..)
                     , getProjectInfo
                     ) where


-- | Project data
data Project = Project FilePath Metadata
                deriving (Show) 

data Metadata = Metadata { groupId :: String
                         , projectId :: String
                         , name :: String
                         , description :: String
                         } deriving (Show)

-- | Get project info
getProjectInfo :: Project -> String
getProjectInfo (Project src md) = "Project location: " ++ src ++ "\n" ++
                                  "Project name: " ++ name md ++ "\n" ++
                                  "Sources: " -- ++ projectSrcPath pom ++ "\n" ++
                                  -- "Tests: " ++ projectTestPath pom ++ "\n"
