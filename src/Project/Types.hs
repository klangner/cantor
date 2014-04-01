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
                     , buildPackageGraph 
                     , scp ) where

import Control.Arrow
import Data.Graph
import Data.Tree
import qualified Data.Map as M


type Dependency = (Int, Int)
data DependencyGraph = PackageGraph [String] Graph deriving (Show) --[Dependency] deriving (Show)

-- | Project data
data Project = Project { projectPath :: FilePath
                       , projectPackages :: DependencyGraph } deriving (Show)
                       
                       
-- | Build dependency graph from node and edges given as names
buildPackageGraph :: [String] -> [(String, String)] -> DependencyGraph
buildPackageGraph ns es = PackageGraph ns $ buildG (0, length ns - 1) $ map ((M.!) ids *** (M.!) ids) es
    where ids = M.fromList (zip ns [0..]) 
    
    
-- Returns list of strongly connected packages    
scp :: DependencyGraph -> [[String]]
scp (PackageGraph pkgs graph) = map mkPkgGroup groups
        where groups = filter (not . null . subForest ) (scc graph)
              mkPkgGroup node = nodeName node : concatMap mkPkgGroup (subForest node)
              nodeName node = pkgs !! rootLabel node
