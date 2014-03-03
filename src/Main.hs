{- |
Module : Main
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Main GUI application for gathering and presenting information..
-}
module Main where

import System.Environment
import MainFrame
import Utils.Folder
import Maven.Pom as Pom
import Report


main::IO ()
main = do
    (path:_) <- getArgs
    putStrLn $ "Project path: " ++ path
    report <- analyzePom (joinPaths path "pom.xml")
    ui <- runGuiApp
    displayReport ui report
    return ()

    
-- | Load POM file and analyze it    
analyzePom :: FilePath -> IO Report
analyzePom f = do 
    pom <- Pom.load f
    return $ Report "Maven" 
        [ Report "About" [ KeyValue "Name" (projectName pom)
                         , KeyValue "Description" (projectDesc pom)
                         , KeyValue "Version" (projectVersion pom)
                         ]
        ] 

    
