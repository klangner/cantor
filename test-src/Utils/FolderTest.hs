{- |
Module : Utils.FolderTest
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable
-}

module Utils.FolderTest (testCases) where

import Utils.Folder
import Test.HUnit


testCases :: [(String, Test)]
testCases = [ ( "There are 2 files", TestCase $ prop_countFiles 3 "fixtures/Utils/Folder")
            ]
         
-- | Check function listing files in directory         
prop_countFiles :: Int -> String -> Assertion         
prop_countFiles e folder = do 
    files <- listFiles folder
    assertEqual ("Count files in: " ++ folder) e (length files)          
