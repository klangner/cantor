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
import Graphics.UI.Gtk
import Control.Monad.Trans(liftIO)
import Utils.Folder
import Maven.Pom as Pom
import Report


main::IO ()
main = do
    (path:_) <- getArgs
    putStrLn $ "Project path: " ++ path
    report <- analyzePom (joinPaths path "pom.xml")
    showReport report

    
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

    
-- | Open GUI window with report data    
showReport :: Report -> IO ()
showReport _ = do
    _ <- initGUI
    window <- windowNew
    _ <- window `on` deleteEvent $ liftIO mainQuit >> return False
    widgetShowAll window
    mainGUI
