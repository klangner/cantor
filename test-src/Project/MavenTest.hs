{- |
Module : Project.MavenTest
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable
-}

module Project.MavenTest (testCases) where

import Project.Maven as Pom
import Test.HUnit


testCases :: [(String, Test)]
testCases = [ ( "No POM", TestCase $ prop_pomExists False "fixtures/Maven/invalid")
            , ( "POM exists", TestCase $ prop_pomExists True "fixtures/Maven/project1/pom.xml")
            , ( "Project name", TestCase $ prop_projectName "Demo project" "fixtures/Maven/project1/pom.xml")
            , ( "Project description", TestCase $ prop_projectDesc "Same description text." "fixtures/Maven/project1/pom.xml")
            , ( "Project version", TestCase $ prop_projectVersion "0.0.1-SNAPSHOT" "fixtures/Maven/project1/pom.xml")
            ]
         
-- | Joining paths         
prop_pomExists :: Bool -> FilePath -> Assertion         
prop_pomExists e fp = do
    pom <- load fp
    assertEqual fp e (isValid pom)          
    
-- | Get project name
prop_projectName :: String -> FilePath -> Assertion         
prop_projectName name fp = do
    pom <- load fp
    assertEqual fp name (projectName pom)              
    
-- | Get project description      
prop_projectDesc :: String -> FilePath -> Assertion         
prop_projectDesc name fp = do
    pom <- load fp
    assertEqual fp name (projectDesc pom)
    
-- | Get project version      
prop_projectVersion :: String -> FilePath -> Assertion         
prop_projectVersion name fp = do
    pom <- load fp
    assertEqual fp name (projectVersion pom)                                        