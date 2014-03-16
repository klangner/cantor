{- |
Module : Utils.List
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Helper module with functions operating on lists
-}
module Utils.List ( unique 
                  , splitByLast )where

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