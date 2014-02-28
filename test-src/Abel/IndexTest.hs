{- |
Module : Condor.Search.IndexTest
Copyright : Copyright (C) 2013-2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable
-}

module Abel.IndexTest (testCases) where

import Test.HUnit


testCases :: [(String, Test)]
testCases = [ ( "Empty index has length 0"
              , TestCase prop_empty)
            
        ]
         
-- | Check empty index         
prop_empty :: Assertion         
prop_empty = assertEqual "Empty index has 0 size" 0 0          
