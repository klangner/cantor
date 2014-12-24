{- |
Module : Cantor.Analysis.Dependency
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3
Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Analysis of dependecy graph between project packages.

-}
module Cantor.Analysis.Dependency ( DependencyGraph
--                                  , buildPackageGraph
                                  , scp ) where

import Control.Arrow
import Data.Graph (Graph, buildG, scc)
import Data.Tree
import System.FilePath
import qualified Data.Map as M
import Cantor.Project (Project)


data DependencyGraph = DG [String] Graph deriving (Show) --[Dependency] deriving (Show)

{-

-- | Build dependency graph for given project.from node and edges given as names
buildPackageGraph :: Project -> DependencyGraph
buildPackageGraph prj = buildGraph ()

-- Helper function for building a graph from nodes and edges
buildGraph :: [String]              -- Node names
           -> [(String, String)]    -- Edges
           -> DependencyGraph
buildGraph ns es = DG ns $ buildG (0, length ns - 1) $ map ((M.!) ids *** (M.!) ids) es
    where ids = M.fromList (zip ns [0..])

-}
-- Returns list of strongly connected packages
scp :: DependencyGraph -> [[String]]
scp (DG pkgs graph) = map mkPkgGroup groups
        where groups = filter (not . null . subForest ) (scc graph)
              mkPkgGroup node = nodeName node : concatMap mkPkgGroup (subForest node)
              nodeName node = pkgs !! rootLabel node
