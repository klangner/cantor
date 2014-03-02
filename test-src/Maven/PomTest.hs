{- |
Module : Maven.PomTest
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable
-}

module Maven.PomTest (testCases) where

import Maven.Pom as Pom
import Test.HUnit


testCases :: [(String, Test)]
testCases = [ ( "No POM", TestCase $ prop_pomExists False "fixtures/Maven/invalid")
            , ( "POM exists", TestCase $ prop_pomExists True "fixtures/Maven/project1/pom.xml")
            , ( "Project name", TestCase $ prop_projectName "Demo project" "fixtures/Maven/project1/pom.xml")
            ]
         
-- | Joining paths         
prop_pomExists :: Bool -> FilePath -> Assertion         
prop_pomExists e fp = do
    pom <- load fp
    assertEqual fp e (isValid pom)          
    
-- | Joining paths         
prop_projectName :: String -> FilePath -> Assertion         
prop_projectName name fp = do
    pom <- load fp
    assertEqual fp name (projectName pom)              