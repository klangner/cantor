{- |
Module : Utils.Graph
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Dependency analysis
-}
module Utils.Graph ( Graph(..) 
                   , NamesGraph 
                   , paths
                   , simplifyNames ) where

import Control.Arrow
import Utils.List (commonPrefix)


-- | Graph data type 
data Graph a = Graph [a] [(a, a)] 
instance Show a => Show (Graph a) where
    show (Graph vs es) = "* Vertices: \n" ++ vertices ++ "* Edges: \n" ++ edges ++ "\n"
        where vertices = concatMap (\x -> show x ++ "\n") vs
              edges = concatMap (\(x,y) -> show x ++ " -> " ++ show y ++ "\n") es
              

-- | Specialized graph with names
type NamesGraph = Graph String


-- | Simplify graph names by removing common prefix
simplifyNames :: NamesGraph -> NamesGraph
simplifyNames (Graph vs es) = Graph vs' es' 
    where prefix = commonPrefix vs
          n = length prefix
          vs' = map (drop n) vs
          es' = map (drop n *** drop n) es
          
          
-- Find all paths from given starting element
paths :: Eq a => a -> [(a,a)] -> [[a]]
paths _ [] = []
paths x ys = concatMap f (hasSource x ys)
    where rest e = filter (e /=) ys
          f (_,b) = map (\z -> b : z) ([]:subPaths b)
          subPaths b2 = paths b2 (rest (x,b2))

-- | Filter only those edges which has given source
hasSource :: Eq a => a -> [(a,a)] -> [(a,a)]
hasSource x = filter (\(a, _) -> a == x)    
              