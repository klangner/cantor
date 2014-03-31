{- |
Module : Project.Types
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Project model
-}
module Project.Types ( Dependency
                     , DependencyGraph(..)
                     , Project(..) 
                     , buildPackageGraph ) where

import Control.Arrow
import qualified Data.Map as M


type Dependency = (Int, Int)
data DependencyGraph = PackageGraph [String] [Dependency] deriving (Show)

-- | Project data
data Project = Project { projectPath :: FilePath
                       , projectPackages :: DependencyGraph } deriving (Show)
                       
                       
-- | Build dependency graph from node and edges given as names
buildPackageGraph :: [String] -> [(String, String)] -> DependencyGraph
buildPackageGraph ns es = PackageGraph ns $ map ((M.!) ids *** (M.!) ids) es
    where ids = M.fromList (zip ns [1..]) 
    
    