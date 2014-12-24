{- |
Module : Cantor.Utils.List
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Helper module with functions operating on lists
-}
module Cantor.Utils.List ( commonPrefix 
                         , simplifyNames
                         , splitByLast 
                         , takePrefix
                         , unique ) where

import Data.Set (fromList, toList)
import Data.List (intercalate)
import Data.List.Split (splitOn)


-- Remove duplicates from list
unique :: Ord a => [a] -> [a]              
unique = toList . fromList 


-- | Split by last given element
splitByLast :: Eq a => [a] -> [a] -> ([a], [a])
splitByLast sep xs = (intercalate sep (init tokens), last tokens)
    where tokens = splitOn sep xs

    
-- | find common prefix in list elements
commonPrefix :: Eq a => [[a]] -> [a]
commonPrefix [] = []
commonPrefix [x] = x
commonPrefix (x:xs) = takePrefix x (commonPrefix xs)
 

-- | Take common prefix from 2 lists
takePrefix :: Eq a => [a] -> [a] -> [a]
takePrefix (x:xs) (y:ys)
    | x == y    = x : takePrefix xs ys
takePrefix _ _ = []


-- | Simplify names by removing common prefix
simplifyNames :: [String] -> [String]
simplifyNames xs = map (drop n) xs 
    where prefix = commonPrefix xs
          n = length prefix
