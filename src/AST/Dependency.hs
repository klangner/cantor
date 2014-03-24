{- |
Module : AST.Dependency
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Dependency analysis
-}
module AST.Dependency ( packageGraph) where

import Utils.List (unique)
import Utils.Graph
import AST.Parser.JavaParser (parseProject)
import AST.Model (ImportDecl(..), packageName, packageImports)


-- | Build package dependency graph. 
packageGraph :: FilePath -> IO NamesGraph 
packageGraph src = do 
    pkgs <- parseProject src
    let names = unique $ map packageName pkgs
    let depends = unique $ concatMap f pkgs
    let graph = removeExternalDepends $ Graph names depends 
    return graph
        where f p = map (\(ImportDecl x _) -> (packageName p, x)) (packageImports p)


-- | Remove from graph external dependencies
removeExternalDepends :: NamesGraph -> NamesGraph
removeExternalDepends (Graph vs es) = Graph vs $ filter (\(_,y) -> y `elem` vs) es


