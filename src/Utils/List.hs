{- |
Module : Utils.List
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Helper module with functions operating on lists
-}
module Utils.List ( commonPrefix 
                  , takePrefix
                  , splitByLast 
                  , unique )where

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