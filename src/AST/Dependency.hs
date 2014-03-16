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
                      , packages 
                      , removeExternalDepends) where

import Utils.List (unique)
import AST.Parser.JavaParser (parseProject)
import AST.Model (ImportDecl(..), packageName, packageImports)


-- | Dependency graph 
data Graph = Graph [Vertice] [Edge] deriving (Show)

type Vertice = String
type Edge = (String, String)

-- | Build package dependency graph. 
packages :: FilePath -> IO Graph 
packages src = do 
    pkgs <- parseProject src
    let names = unique $ map packageName pkgs
    let depends = unique $ concatMap f pkgs
    return $ Graph names depends
        where f p = map (\(ImportDecl x _) -> (packageName p, x)) (packageImports p)


-- | Remove from graph external dependencies
removeExternalDepends :: Graph -> Graph
removeExternalDepends (Graph vs es) = Graph vs $ filter (\(_,y) -> y `elem` vs) es