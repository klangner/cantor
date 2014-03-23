{- |
Module : AST.Dependency
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Dependency analysis
-}
module AST.Dependency ( Graph(..)
                      , packageGraph) where

import Utils.List (unique)
import AST.Parser.JavaParser (parseProject)
import AST.Model (ImportDecl(..), packageName, packageImports)


-- | Dependency graph 
data Graph = Graph [Vertice] [Edge] 
instance Show Graph where
    show (Graph vs es) = "Vertices: \n" ++ vertices ++ "\nEdges: \n" ++ edges ++ "\n"
        where vertices = unlines vs
              edges = concatMap (\(x,y) -> x ++ " -> " ++ y ++ "\n") es
              

type Vertice = String
type Edge = (String, String)

-- | Build package dependency graph. 
packageGraph :: FilePath -> IO Graph 
packageGraph src = do 
    pkgs <- parseProject src
    let names = unique $ map packageName pkgs
    let depends = unique $ concatMap f pkgs
    let graph = removeExternalDepends $ Graph names depends 
    return graph
        where f p = map (\(ImportDecl x _) -> (packageName p, x)) (packageImports p)


-- | Remove from graph external dependencies
removeExternalDepends :: Graph -> Graph
removeExternalDepends (Graph vs es) = Graph vs $ filter (\(_,y) -> y `elem` vs) es


