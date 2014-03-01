{- |
Module : Maven.PomTest
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable
-}

module Maven.PomTest (testCases) where

import Maven.Pom
import Test.HUnit


testCases :: [(String, Test)]
testCases = [ ( "No POM", TestCase $ prop_pomExists False "fixtures/Maven/invalid")
            , ( "POM exists", TestCase $ prop_pomExists True "fixtures/Maven/project1/pom.xml")
            ]
         
-- | Joining paths         
prop_pomExists :: Bool -> FilePath -> Assertion         
prop_pomExists e fp = do
    pom <- loadPom fp
    assertEqual fp e (isValidPom pom)          