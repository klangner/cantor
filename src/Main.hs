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
import Utils.Folder
import Maven.Pom as Pom
import Report
import Graphics.UI.Threepenny       as UI
--import Graphics.UI.Threepenny.Core


main::IO ()
main = do
    (path:_) <- getArgs
    putStrLn $ "Project path: " ++ path
    _ <- analyzePom (joinPaths path "pom.xml")
    startGUI defaultConfig { tpStatic = Just "./report" } setup    
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
    
setup :: Window -> UI ()
setup window = do    
    _ <- return window # set UI.title "Hello World!"
    b1 <- UI.button # set UI.text "Click me!"
    _ <- getBody window #+ [element b1]
    on UI.click b1 $ const $ element b1 # set UI.text "I have been clicked!"